{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Authentication credentials for @signin@ and @signup@. The constructors
-- mirror SurrealDB's auth levels: root, namespace and database system users,
-- and access method based system, bearer and record auth.
module Database.Surreal.Auth
  ( Auth (..)
  , authToValue
  ) where

import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text)

import           Database.Surreal.Types (SurrealValue (..))

-- | A set of credentials. Pass to @signin@; @signup@ takes 'RecordAccess'.
data Auth
  = Root Text Text
    -- ^ Root user: username, password.
  | Namespace Text Text Text
    -- ^ Namespace user: namespace, username, password.
  | Database Text Text Text Text
    -- ^ Database user: namespace, database, username, password.
  | SystemAccess (Maybe Text) (Maybe Text) Text Text Text
    -- ^ Access method system user: namespace, database, access, username, password.
  | BearerAccess (Maybe Text) (Maybe Text) Text Text
    -- ^ Bearer access: namespace, database, access, key.
  | RecordAccess (Maybe Text) (Maybe Text) Text (Map Text SurrealValue)
    -- ^ Record access: namespace, database, access, sign in or sign up variables.
  deriving (Eq, Show)

-- | Render credentials to the object value sent in the @signin@ or @signup@
-- params.
authToValue :: Auth -> SurrealValue
authToValue = VObject . authObject

authObject :: Auth -> Map Text SurrealValue
authObject = \case
  Root user pass ->
    Map.fromList [("user", VString user), ("pass", VString pass)]
  Namespace ns user pass ->
    Map.fromList [("ns", VString ns), ("user", VString user), ("pass", VString pass)]
  Database ns db user pass ->
    Map.fromList [("ns", VString ns), ("db", VString db), ("user", VString user), ("pass", VString pass)]
  SystemAccess ns db ac user pass ->
    withScope ns db (Map.fromList [("ac", VString ac), ("user", VString user), ("pass", VString pass)])
  BearerAccess ns db ac key ->
    withScope ns db (Map.fromList [("ac", VString ac), ("key", VString key)])
  RecordAccess ns db ac vars ->
    withScope ns db (Map.insert "ac" (VString ac) vars)
  where
    withScope ns db base =
      maybe id (Map.insert "ns" . VString) ns
        (maybe id (Map.insert "db" . VString) db base)
