{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The connection handle and its mutable state. 'invoke' is the single
-- chokepoint through which every RPC passes: it enforces the request timeout,
-- routes to the active engine and keeps the session state current so the HTTP
-- transport can attach the right headers and a reconnect can replay the
-- session.
module Database.Surreal.Connection
  ( Surreal (..)
  , LiveSink (..)
  , connect
  , close
  , withSurreal
  , invoke
  , invokeUnit
  , surrealCaps
  , requireCap
  , registerLive
  , unregisterLive
  ) where

import           Control.Exception        (bracket, throwIO)
import           Control.Monad            (unless, void)
import           Control.Concurrent.STM
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.IORef
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Text                (Text)
import           System.Timeout           (timeout)

import           Database.Surreal.Codec
import           Database.Surreal.Engine
import           Database.Surreal.Engine.Http      (connectHttp)
import           Database.Surreal.Engine.WebSocket (connectWebSocket)
import           Database.Surreal.Error
import           Database.Surreal.RPC
import           Database.Surreal.Types

-- | A live query subscriber sink.
newtype LiveSink = LiveSink (TQueue Notification)

-- | A connection to SurrealDB. Construct it with 'connect' or 'withSurreal'.
data Surreal = Surreal
  { sEngine   :: !Engine
  , sCodec    :: !Codec
  , sEndpoint :: !Endpoint
  , sSession  :: !(IORef SessionState)
  , sLive     :: !(TVar (Map Uuid LiveSink))
  , sTimeout  :: !Int
  }

-- | The capabilities of the active transport.
surrealCaps :: Surreal -> EngineCaps
surrealCaps = engCaps . sEngine

-- | Throw 'UnsupportedByTransport' unless the capability is present.
requireCap :: MonadIO m => Text -> Bool -> m ()
requireCap method ok = liftIO (unless ok (throwIO (UnsupportedByTransport method)))

-- | Open a connection to the given URL. Accepts the @ws@, @wss@, @http@ and
-- @https@ schemes. If a namespace or database is set in the options it is
-- selected immediately.
connect :: MonadIO m => Text -> ConnectOpts -> m Surreal
connect url opts = liftIO $ do
  ep <- either (throwIO . ConnectErr) pure (parseEndpoint url)
  let codec = coCodec opts
  sessionRef <- newIORef emptySession
    { ssNamespace = coNamespace opts
    , ssDatabase  = coDatabase opts
    }
  liveRef <- newTVarIO Map.empty
  engine  <- case epTransport ep of
    TransportHttp -> connectHttp ep codec sessionRef
    TransportWs   -> connectWebSocket ep codec (routeNotification liveRef)
  let h = Surreal engine codec ep sessionRef liveRef (coTimeoutMicros opts)
  case (coNamespace opts, coDatabase opts) of
    (Nothing, Nothing) -> pure ()
    (ns, db)           -> void (invoke h "use" [maybeStr ns, maybeStr db])
  pure h

-- | Close a connection and release its transport.
close :: MonadIO m => Surreal -> m ()
close h = liftIO (engClose (sEngine h))

-- | Open a connection, run an action with it, then close it, even on error.
withSurreal :: (MonadIO m) => Text -> ConnectOpts -> (Surreal -> IO a) -> m a
withSurreal url opts act = liftIO (bracket (connect url opts) close act)

-- | Send an RPC and return its result. Enforces the timeout and keeps the
-- session state current. Throws 'SurrealError' on failure.
invoke :: MonadIO m => Surreal -> MethodName -> [SurrealValue] -> m SurrealValue
invoke h method params = liftIO $
  case (method, epTransport (sEndpoint h)) of
    ("use", TransportHttp) -> VNull <$ applyUse h params
    _ -> do
      result <- withTimeout (sTimeout h) method (engRequest (sEngine h) method params)
      updateSession h method params result
      pure result

-- | Like 'invoke' but discards the result.
invokeUnit :: MonadIO m => Surreal -> MethodName -> [SurrealValue] -> m ()
invokeUnit h method params = void (invoke h method params)

withTimeout :: Int -> MethodName -> IO a -> IO a
withTimeout micros method act
  | micros <= 0 = act
  | otherwise   = timeout micros act >>= maybe (throwIO (TimeoutErr method micros)) pure

-- Session bookkeeping --------------------------------------------------------

updateSession :: Surreal -> MethodName -> [SurrealValue] -> SurrealValue -> IO ()
updateSession h method params result = case method of
  "use"          -> applyUse h params
  "signin"       -> storeToken h result
  "signup"       -> storeToken h result
  "authenticate" -> case params of
                      (VString t : _) -> setToken h (Just t)
                      _               -> pure ()
  "invalidate"   -> setToken h Nothing
  "let"          -> case params of
                      [VString k, v] -> modSession h (\s -> s { ssVars = Map.insert k v (ssVars s) })
                      _              -> pure ()
  "set"          -> case params of
                      [VString k, v] -> modSession h (\s -> s { ssVars = Map.insert k v (ssVars s) })
                      _              -> pure ()
  "unset"        -> case params of
                      (VString k : _) -> modSession h (\s -> s { ssVars = Map.delete k (ssVars s) })
                      _               -> pure ()
  _              -> pure ()

applyUse :: Surreal -> [SurrealValue] -> IO ()
applyUse h params = case params of
  [ns, db] -> modSession h (\s -> s { ssNamespace = strOf ns `orKeep` ssNamespace s
                                     , ssDatabase  = strOf db `orKeep` ssDatabase s })
  _        -> pure ()
  where
    orKeep (Just x) _   = Just x
    orKeep Nothing  old = old

storeToken :: Surreal -> SurrealValue -> IO ()
storeToken h result = setToken h (tokenOf result)
  where
    tokenOf (VString t)  = Just t
    tokenOf (VObject m)  = case Map.lookup "access" m of
                             Just (VString t) -> Just t
                             _                -> case Map.lookup "token" m of
                                                   Just (VString t) -> Just t
                                                   _                -> Nothing
    tokenOf _            = Nothing

setToken :: Surreal -> Maybe Text -> IO ()
setToken h t = modSession h (\s -> s { ssToken = t })

modSession :: Surreal -> (SessionState -> SessionState) -> IO ()
modSession h f = atomicModifyIORef' (sSession h) (\s -> (f s, ()))

maybeStr :: Maybe Text -> SurrealValue
maybeStr = maybe VNull VString

strOf :: SurrealValue -> Maybe Text
strOf (VString t) = Just t
strOf _           = Nothing

-- Live query routing ---------------------------------------------------------

routeNotification :: TVar (Map Uuid LiveSink) -> Notification -> IO ()
routeNotification liveRef n = do
  m <- readTVarIO liveRef
  case Map.lookup (ntfQueryId n) m of
    Just (LiveSink q) -> atomically (writeTQueue q n)
    Nothing           -> pure ()

-- | Register a live query sink for the given query id and return its queue.
registerLive :: Surreal -> Uuid -> IO (TQueue Notification)
registerLive h qid = do
  q <- newTQueueIO
  atomically (modifyTVar' (sLive h) (Map.insert qid (LiveSink q)))
  pure q

-- | Remove a live query sink.
unregisterLive :: Surreal -> Uuid -> IO ()
unregisterLive h qid = atomically (modifyTVar' (sLive h) (Map.delete qid))
