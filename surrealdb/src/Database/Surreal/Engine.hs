{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The transport abstraction. Both the WebSocket and HTTP engines expose the
-- same request interface so the RPC, value and method layers are written once.
-- Capability flags let the method layer fail fast on operations a transport
-- cannot perform, for example live queries over HTTP.
module Database.Surreal.Engine
  ( -- * Engine
    Engine (..)
  , MethodName
  , EngineCaps (..)
  , wsCaps
  , httpCaps

    -- * Endpoints
  , Endpoint (..)
  , Transport (..)
  , parseEndpoint

    -- * Connect options
  , ConnectOpts (..)
  , defaultConnectOpts

    -- * Session state
  , SessionState (..)
  , emptySession
  ) where

import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Network.URI            (URI (..), URIAuth (..), parseURI)

import           Database.Surreal.Codec (Codec (..))
import           Database.Surreal.Error
import           Database.Surreal.Types (SurrealValue)

-- | The name of an RPC method, for example @select@.
type MethodName = Text

-- | A live transport. 'engRequest' performs one request and returns its result
-- or throws a 'SurrealError'. Notifications, when supported, are delivered to a
-- callback supplied at construction time, not through this record.
data Engine = Engine
  { engRequest :: MethodName -> [SurrealValue] -> IO SurrealValue
  , engCaps    :: EngineCaps
  , engClose   :: IO ()
  }

-- | What a transport can do.
data EngineCaps = EngineCaps
  { capsLive        :: !Bool
  , capsTransaction :: !Bool
  , capsSessionVars :: !Bool
  } deriving (Eq, Show)

-- | The WebSocket transport supports everything.
wsCaps :: EngineCaps
wsCaps = EngineCaps True True True

-- | The HTTP transport is stateless: no live queries, transactions or session
-- scoped variables.
httpCaps :: EngineCaps
httpCaps = EngineCaps False False False

-- | Which wire transport an endpoint uses.
data Transport = TransportWs | TransportHttp
  deriving (Eq, Show)

-- | A parsed connection endpoint.
data Endpoint = Endpoint
  { epTransport :: !Transport
  , epSecure    :: !Bool
  , epHost      :: !Text
  , epPort      :: !Int
  , epPath      :: !Text
  } deriving (Eq, Show)

-- | Parse a connection URL. Accepts the @ws@, @wss@, @http@ and @https@
-- schemes. When the WebSocket path is empty it defaults to @\/rpc@.
parseEndpoint :: Text -> Either ConnectError Endpoint
parseEndpoint raw =
  case parseURI (T.unpack raw) of
    Nothing  -> Left (BadEndpoint ("could not parse URL: " <> raw))
    Just uri ->
      case uriAuthority uri of
        Nothing   -> Left (BadEndpoint ("URL has no host: " <> raw))
        Just auth -> do
          (transport, secure, defPort) <- scheme (uriScheme uri)
          let host = T.pack (uriRegName auth)
              port = case uriPort auth of
                       (':' : ds) | [(n, "")] <- reads ds -> n
                       _                                   -> defPort
              path = let p = T.pack (uriPath uri)
                     in if T.null p || p == "/" then "/rpc" else p
          Right (Endpoint transport secure host port path)
  where
    scheme = \case
      "ws:"    -> Right (TransportWs, False, 80)
      "wss:"   -> Right (TransportWs, True, 443)
      "http:"  -> Right (TransportHttp, False, 80)
      "https:" -> Right (TransportHttp, True, 443)
      other    -> Left (BadEndpoint ("unsupported scheme: " <> T.pack other))

-- | Options controlling how a connection is established.
data ConnectOpts = ConnectOpts
  { coCodec         :: !Codec
  , coNamespace     :: !(Maybe Text)
  , coDatabase      :: !(Maybe Text)
  , coTimeoutMicros :: !Int
  , coReconnect     :: !Bool
  } deriving (Eq, Show)

-- | Sensible defaults: CBOR codec, no namespace or database preselected, a
-- thirty second request timeout and reconnection enabled.
defaultConnectOpts :: ConnectOpts
defaultConnectOpts = ConnectOpts
  { coCodec         = CodecCbor
  , coNamespace     = Nothing
  , coDatabase      = Nothing
  , coTimeoutMicros = 30 * 1000000
  , coReconnect     = True
  }

-- | The mutable session state tracked per connection. It is used to attach
-- authentication and namespace headers on the stateless HTTP transport and to
-- replay the session after a WebSocket reconnect.
data SessionState = SessionState
  { ssToken     :: !(Maybe Text)
  , ssNamespace :: !(Maybe Text)
  , ssDatabase  :: !(Maybe Text)
  , ssVars      :: !(Map Text SurrealValue)
  } deriving (Eq, Show)

-- | A fresh, unauthenticated session.
emptySession :: SessionState
emptySession = SessionState Nothing Nothing Nothing Map.empty
