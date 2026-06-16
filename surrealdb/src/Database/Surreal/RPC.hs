{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The JSON-RPC shaped envelope shared by both transports and both codecs.
-- Requests, responses and live notifications are all expressed in terms of
-- 'SurrealValue', so a single definition serves CBOR and JSON alike; only the
-- codec function differs.
module Database.Surreal.RPC
  ( RpcId
  , RpcRequest (..)
  , RpcResponse (..)
  , Notification (..)
  , Action (..)
  , RpcFrame (..)
  , requestToValue
  , parseFrame
  ) where

import qualified Data.Map.Strict        as Map
import           Data.Text              (Text)
import qualified Data.Vector            as V
import           Data.Word              (Word64)

import           Database.Surreal.Error
import           Database.Surreal.Types

-- | A monotonic request id. Sent on the wire as an integer.
type RpcId = Word64

-- | An outgoing RPC request.
data RpcRequest = RpcRequest
  { reqId     :: !RpcId
  , reqMethod :: !Text
  , reqParams :: ![SurrealValue]
  } deriving (Eq, Show)

-- | A response correlated to a request by id.
data RpcResponse = RpcResponse
  { resId     :: !RpcId
  , resResult :: !(Either RpcError SurrealValue)
  } deriving (Eq, Show)

-- | A live query notification. Delivered over WebSocket without a request id.
data Notification = Notification
  { ntfQueryId  :: !Uuid
  , ntfAction   :: !Action
  , ntfRecordId :: !(Maybe RecordId)
  , ntfValue    :: !SurrealValue
  } deriving (Eq, Show)

-- | The kind of change a live notification reports.
data Action = ActionCreate | ActionUpdate | ActionDelete | ActionKilled
  deriving (Eq, Show)

-- | A decoded incoming frame: either a response or a live notification.
data RpcFrame
  = FrameResponse RpcResponse
  | FrameNotification Notification
  deriving (Eq, Show)

-- | Encode a request as the value the codec will serialise.
requestToValue :: RpcRequest -> SurrealValue
requestToValue (RpcRequest i m ps) = VObject $ Map.fromList
  [ ("id",     VInt (fromIntegral i))
  , ("method", VString m)
  , ("params", VArray (V.fromList ps))
  ]

-- | Classify and parse a decoded incoming value into a frame.
parseFrame :: SurrealValue -> Either ProtocolError RpcFrame
parseFrame (VObject o) =
  case Map.lookup "id" o of
    Just (VInt i) ->
      case (Map.lookup "result" o, Map.lookup "error" o) of
        (_, Just err)        -> FrameResponse . RpcResponse (fromIntegral i) . Left <$> parseError err
        (Just result, _)     -> Right (FrameResponse (RpcResponse (fromIntegral i) (Right result)))
        (Nothing, Nothing)   -> Left MissingResultAndError
    _ ->
      -- No top level integer id: this is a live notification carried in result.
      case Map.lookup "result" o of
        Just (VObject n) -> FrameNotification <$> parseNotification n
        _                -> Left (UnexpectedFrame "frame has neither a response id nor a notification body")
parseFrame _ = Left (UnexpectedFrame "top level frame is not an object")

parseError :: SurrealValue -> Either ProtocolError RpcError
parseError (VObject e) =
  let code = case Map.lookup "code" e of
               Just (VInt c) -> fromIntegral c :: Int
               _             -> 0
      msg  = case Map.lookup "message" e of
               Just (VString m) -> m
               _                -> "unknown error"
  in Right (RpcError code msg)
parseError _ = Left (UnexpectedFrame "error field is not an object")

parseNotification :: Map.Map Text SurrealValue -> Either ProtocolError Notification
parseNotification n = do
  qid <- case firstJust [Map.lookup "id" n, Map.lookup "queryId" n] of
           Just (VUuid u)   -> Right u
           Just (VString s) -> maybe (Left (UnexpectedFrame "bad notification uuid")) Right (uuidFromText s)
           _                -> Left (UnexpectedFrame "notification missing query id")
  act <- case Map.lookup "action" n of
           Just (VString a) -> parseAction a
           _                -> Left (UnexpectedFrame "notification missing action")
  let value = firstJustValue [Map.lookup "result" n, Map.lookup "value" n]
      rid   = case value of
                VObject m -> case Map.lookup "id" m of
                               Just (VRecordId r) -> Just r
                               _                  -> Nothing
                _         -> Nothing
  Right (Notification qid act rid value)

parseAction :: Text -> Either ProtocolError Action
parseAction = \case
  "CREATE" -> Right ActionCreate
  "UPDATE" -> Right ActionUpdate
  "DELETE" -> Right ActionDelete
  "KILLED" -> Right ActionKilled
  other    -> Left (UnexpectedFrame ("unknown live action " <> other))

firstJust :: [Maybe a] -> Maybe a
firstJust = foldr (\x acc -> maybe acc Just x) Nothing

firstJustValue :: [Maybe SurrealValue] -> SurrealValue
firstJustValue xs = maybe VNull id (firstJust xs)
