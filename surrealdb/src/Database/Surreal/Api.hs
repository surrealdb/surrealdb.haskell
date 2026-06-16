{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The typed RPC methods. Every function is written against 'MonadSurreal' so
-- it runs in any monad that carries the connection, and against
-- 'SurrealRecord' so results decode straight into application types. None of
-- these signatures can name a codec, which is what makes the methods codec
-- agnostic.
module Database.Surreal.Api
  ( -- * Targets
    IsTarget (..)
  , onTable
  , onRecord
  , onRange

    -- * Session and auth
  , use
  , signin
  , signup
  , authenticate
  , invalidate
  , letVar
  , unset

    -- * Queries
  , StatementResult (..)
  , query
  , queryFirst
  , statementRows

    -- * CRUD
  , select
  , create
  , insert
  , update
  , upsert
  , merge
  , patch
  , delete

    -- * Graph
  , relate

    -- * Functions and info
  , run
  , version
  , info
  , health
  , ping
  ) where

import           Control.Exception      (throwIO)
import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text)
import qualified Data.Vector            as V

import           Database.Surreal.Auth
import           Database.Surreal.Class
import           Database.Surreal.Connection
import           Database.Surreal.Engine     (MethodName)
import           Database.Surreal.Error
import           Database.Surreal.Monad
import           Database.Surreal.Types

-- Targets --------------------------------------------------------------------

-- | Anything that can address one or more records: a table, a record id, a
-- record id range, or a table name given as 'Text'.
class IsTarget a where
  toTarget :: a -> Target

instance IsTarget Target        where toTarget = id
instance IsTarget Table         where toTarget = TargetTable
instance IsTarget RecordId      where toTarget = TargetRecord
instance IsTarget RecordIdRange where toTarget = TargetRange
instance IsTarget Text          where toTarget = TargetTable . Table

-- | Address a whole table by name.
onTable :: Text -> Target
onTable = TargetTable . Table

-- | Address a single record.
onRecord :: RecordId -> Target
onRecord = TargetRecord

-- | Address a range of records within a table.
onRange :: Text -> Bound -> Bound -> Target
onRange tb b e = TargetRange (RecordIdRange tb b e)

targetToValue :: Target -> SurrealValue
targetToValue = \case
  TargetTable  t                  -> VTable t
  TargetRecord r                  -> VRecordId r
  TargetRange (RecordIdRange tb b e) -> VRecordId (RecordId tb (VRange (Range b e)))

-- Internal helpers -----------------------------------------------------------

call :: MonadSurreal m => MethodName -> [SurrealValue] -> m SurrealValue
call method params = askSurreal >>= \h -> invoke h method params

callUnit :: MonadSurreal m => MethodName -> [SurrealValue] -> m ()
callUnit method params = void (call method params)

decodeOrThrow :: MonadSurreal m => Either DecodeError a -> m a
decodeOrThrow = either (liftIO . throwIO . DecodeErr) pure

decodeList :: SurrealRecord a => SurrealValue -> Either DecodeError [a]
decodeList = \case
  VArray xs -> traverse fromSurreal (V.toList xs)
  VSet xs   -> traverse fromSurreal (V.toList xs)
  VNull     -> Right []
  VNone     -> Right []
  other     -> fmap pure (fromSurreal other)

decodeOne :: SurrealRecord a => SurrealValue -> Either DecodeError (Maybe a)
decodeOne = \case
  VNull     -> Right Nothing
  VNone     -> Right Nothing
  VArray xs -> case V.toList xs of
                 []      -> Right Nothing
                 (x : _) -> Just <$> fromSurreal x
  other     -> Just <$> fromSurreal other

tokenOrThrow :: MonadSurreal m => SurrealValue -> m Text
tokenOrThrow = \case
  VString t  -> pure t
  VObject m  -> case Map.lookup "access" m of
                  Just (VString t) -> pure t
                  _                -> case Map.lookup "token" m of
                                        Just (VString t) -> pure t
                                        _                -> liftIO (throwIO (AuthErr NotAuthenticated))
  _          -> liftIO (throwIO (AuthErr NotAuthenticated))

-- Session and auth -----------------------------------------------------------

-- | Select a namespace and database. Either may be left unchanged with
-- 'Nothing'.
use :: MonadSurreal m => Maybe Text -> Maybe Text -> m ()
use ns db = callUnit "use" [maybe VNull VString ns, maybe VNull VString db]

-- | Sign in with the given credentials and return the access token.
signin :: MonadSurreal m => Auth -> m Text
signin a = call "signin" [authToValue a] >>= tokenOrThrow

-- | Sign up a new record user and return the access token.
signup :: MonadSurreal m => Auth -> m Text
signup a = call "signup" [authToValue a] >>= tokenOrThrow

-- | Authenticate the connection with an existing token.
authenticate :: MonadSurreal m => Text -> m ()
authenticate t = callUnit "authenticate" [VString t]

-- | Invalidate the current authentication.
invalidate :: MonadSurreal m => m ()
invalidate = callUnit "invalidate" []

-- | Define a session variable usable in later queries as @$name@.
letVar :: MonadSurreal m => Text -> SurrealValue -> m ()
letVar k v = callUnit "let" [VString k, v]

-- | Remove a session variable.
unset :: MonadSurreal m => Text -> m ()
unset k = callUnit "unset" [VString k]

-- Queries --------------------------------------------------------------------

-- | The outcome of one statement in a multi statement query.
data StatementResult = StatementResult
  { srStatus :: !Text
  , srResult :: !SurrealValue
  } deriving (Eq, Show)

-- | Run a SurrealQL query with bound variables and return one result per
-- statement.
query :: MonadSurreal m => Text -> Map Text SurrealValue -> m [StatementResult]
query q vars = call "query" [VString q, VObject vars] >>= decodeOrThrow . parseStatements

-- | Run a query and decode the rows of its first statement.
queryFirst :: (MonadSurreal m, SurrealRecord a) => Text -> Map Text SurrealValue -> m [a]
queryFirst q vars = do
  stmts <- query q vars
  case stmts of
    []      -> pure []
    (s : _) -> do
      assertOk s
      decodeOrThrow (decodeList (srResult s))

-- | Decode the rows of a statement result as a list.
statementRows :: SurrealRecord a => StatementResult -> Either DecodeError [a]
statementRows = decodeList . srResult

assertOk :: MonadSurreal m => StatementResult -> m ()
assertOk s
  | srStatus s == "ERR" =
      liftIO (throwIO (RpcErr (RpcError 0 (asText (srResult s)))))
  | otherwise = pure ()
  where asText (VString t) = t
        asText other       = describeValue other

parseStatements :: SurrealValue -> Either DecodeError [StatementResult]
parseStatements = \case
  VArray xs -> traverse parseStmt (V.toList xs)
  VNull     -> Right []
  other     -> Right [StatementResult "OK" other]
  where
    parseStmt (VObject m) =
      let status = case Map.lookup "status" m of
                     Just (VString s) -> s
                     _                -> "OK"
          result = Map.findWithDefault (Map.findWithDefault VNull "value" m) "result" m
      in Right (StatementResult status result)
    parseStmt other = Right (StatementResult "OK" other)

-- CRUD -----------------------------------------------------------------------

-- | Select records from a table, a single record, or a record id range.
select :: (MonadSurreal m, SurrealRecord a, IsTarget t) => t -> m [a]
select tgt = call "select" [targetToValue (toTarget tgt)] >>= decodeOrThrow . decodeList

-- | Create one or more records with the given content.
create :: (MonadSurreal m, SurrealRecord a, SurrealRecord r, IsTarget t) => t -> a -> m [r]
create tgt dat = call "create" [targetToValue (toTarget tgt), toSurreal dat] >>= decodeOrThrow . decodeList

-- | Insert records, optionally into a named table.
insert :: (MonadSurreal m, SurrealRecord a, SurrealRecord r) => Maybe Table -> [a] -> m [r]
insert mtbl dats =
  call "insert" [maybe VNull VTable mtbl, VArray (V.fromList (map toSurreal dats))]
    >>= decodeOrThrow . decodeList

-- | Replace records with the given content.
update :: (MonadSurreal m, SurrealRecord a, SurrealRecord r, IsTarget t) => t -> a -> m [r]
update tgt dat = call "update" [targetToValue (toTarget tgt), toSurreal dat] >>= decodeOrThrow . decodeList

-- | Create or replace records with the given content.
upsert :: (MonadSurreal m, SurrealRecord a, SurrealRecord r, IsTarget t) => t -> a -> m [r]
upsert tgt dat = call "upsert" [targetToValue (toTarget tgt), toSurreal dat] >>= decodeOrThrow . decodeList

-- | Merge the given content into existing records.
merge :: (MonadSurreal m, SurrealRecord a, SurrealRecord r, IsTarget t) => t -> a -> m [r]
merge tgt dat = call "merge" [targetToValue (toTarget tgt), toSurreal dat] >>= decodeOrThrow . decodeList

-- | Apply JSON Patch operations to records. Each operation is an object value.
patch :: (MonadSurreal m, SurrealRecord r, IsTarget t) => t -> [SurrealValue] -> m [r]
patch tgt ops = call "patch" [targetToValue (toTarget tgt), VArray (V.fromList ops)] >>= decodeOrThrow . decodeList

-- | Delete records.
delete :: (MonadSurreal m, SurrealRecord r, IsTarget t) => t -> m [r]
delete tgt = call "delete" [targetToValue (toTarget tgt)] >>= decodeOrThrow . decodeList

-- Graph ----------------------------------------------------------------------

-- | Create a graph edge from one record to another through an edge table,
-- optionally carrying edge content.
relate
  :: (MonadSurreal m, SurrealRecord a, SurrealRecord r)
  => RecordId -> Text -> RecordId -> Maybe a -> m [r]
relate from edge to mdat =
  call "relate" [VRecordId from, VTable (Table edge), VRecordId to, maybe VNull toSurreal mdat]
    >>= decodeOrThrow . decodeList

-- Functions and info ---------------------------------------------------------

-- | Run a built in or user defined function. The optional version selects a
-- SurrealML model version.
run :: (MonadSurreal m, SurrealRecord a) => Text -> Maybe Text -> [SurrealValue] -> m a
run name mver args =
  call "run" [VString name, maybe VNull VString mver, VArray (V.fromList args)]
    >>= decodeOrThrow . fromSurreal

-- | The server version string.
version :: MonadSurreal m => m Text
version = call "version" [] >>= \case
  VString t -> pure t
  VObject m -> case Map.lookup "version" m of
                 Just (VString t) -> pure t
                 _                -> pure ""
  _         -> pure ""

-- | The currently authenticated record user, if any.
info :: (MonadSurreal m, SurrealRecord a) => m (Maybe a)
info = call "info" [] >>= decodeOrThrow . decodeOne

-- | Check that the server is healthy.
health :: MonadSurreal m => m ()
health = callUnit "health" []

-- | Ping the server.
ping :: MonadSurreal m => m ()
ping = callUnit "ping" []
