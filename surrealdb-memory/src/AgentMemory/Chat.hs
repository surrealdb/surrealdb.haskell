{-# LANGUAGE OverloadedStrings #-}

-- | The chat loop. 'chat' returns the full response; 'chatStream' fetches the
-- server sent event stream and parses it into chunks.
module AgentMemory.Chat
  ( ChatOptions (..)
  , defaultChatOptions
  , chat
  , ChatChunk (..)
  , chatStream
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (Value (..), eitherDecodeStrict)
import qualified Data.Aeson.KeyMap      as KM
import qualified Data.ByteString.Lazy   as BL
import           Data.Maybe             (mapMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE

import           AgentMemory.Client
import           AgentMemory.Types

-- | Options for 'chat'.
data ChatOptions = ChatOptions
  { choSessionId :: !(Maybe Text)
  , choScope     :: !(Maybe Scope)
  } deriving (Eq, Show)

-- | All fields unset.
defaultChatOptions :: ChatOptions
defaultChatOptions = ChatOptions Nothing Nothing

chatBody :: Text -> ChatOptions -> Value
chatBody message opts = objectMaybe
  [ "message"   .=? Just message
  , "sessionId" .=? choSessionId opts
  , "scope"     .=? fmap scopeToJSON (choScope opts)
  ]

-- | Send a chat message and return the full response.
chat :: MonadIO m => AgentMemory -> Text -> ChatOptions -> m Value
chat sp message opts = postJSON sp (contextPath sp "/chat") [] (chatBody message opts)

-- | One frame of a streamed chat response.
data ChatChunk = ChatChunk
  { ccToken     :: !(Maybe Text)
  , ccDone      :: !Bool
  , ccSessionId :: !(Maybe Text)
  , ccRaw       :: !Value
  } deriving (Eq, Show)

-- | Send a chat message and stream the response as parsed chunks. The full
-- stream is read, then parsed; streaming endpoints are not retried.
chatStream :: MonadIO m => AgentMemory -> Text -> ChatOptions -> m [ChatChunk]
chatStream sp message opts = liftIO $ do
  body <- rawRequest sp "POST" (contextPath sp "/chat") [("stream", Just "true")] (Just (chatBody message opts))
  pure (parseSse (TE.decodeUtf8 (BL.toStrict body)))

-- | Parse a server sent event body into chat chunks. Lines beginning with
-- @data:@ carry JSON frames; the @[DONE]@ sentinel is ignored.
parseSse :: Text -> [ChatChunk]
parseSse = mapMaybe parseLine . T.lines
  where
    parseLine line =
      case T.stripPrefix "data:" line of
        Nothing   -> Nothing
        Just rest ->
          let payload = T.strip rest
          in if payload == "[DONE]" || T.null payload
               then Nothing
               else case eitherDecodeStrict (TE.encodeUtf8 payload) of
                      Right v -> Just (toChunk v)
                      Left _  -> Nothing

toChunk :: Value -> ChatChunk
toChunk v@(Object o) = ChatChunk
  { ccToken     = textField "token" `orElse` textField "delta"
  , ccDone      = case KM.lookup "done" o of
                    Just (Bool b) -> b
                    _             -> False
  , ccSessionId = textField "sessionId"
  , ccRaw       = v
  }
  where
    textField k = case KM.lookup k o of
                    Just (String s) -> Just s
                    _               -> Nothing
    orElse (Just x) _ = Just x
    orElse Nothing  y = y
toChunk v = ChatChunk Nothing False Nothing v
