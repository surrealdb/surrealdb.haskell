{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The stateful WebSocket transport. A single reader loop owns the receive
-- side of the socket and correlates each response to its request by id, so many
-- requests may be in flight concurrently over one connection. Live query
-- notifications are handed to the callback supplied at construction.
module Database.Surreal.Engine.WebSocket
  ( connectWebSocket
  ) where

import           Control.Concurrent       (ThreadId, forkIO, killThread)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Exception        (SomeException, catch, throwIO, try)
import           Control.Monad            (void)
import qualified Data.ByteString.Lazy     as BL
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HM
import qualified Data.Text                as T
import qualified Network.WebSockets       as WS
import qualified Wuss                     as WUSS

import           Database.Surreal.Codec
import           Database.Surreal.Engine
import           Database.Surreal.Error
import           Database.Surreal.RPC
import           Database.Surreal.Types (SurrealValue)

type Pending = TVar (HashMap RpcId (TMVar (Either SurrealError SurrealValue)))

-- | Open a WebSocket engine. The notification callback receives every live
-- query notification the server pushes.
connectWebSocket :: Endpoint -> Codec -> (Notification -> IO ()) -> IO Engine
connectWebSocket ep codec onNotif = do
  connVar  <- newEmptyMVar
  readyVar <- newEmptyMVar
  pending  <- newTVarIO HM.empty
  idRef    <- newTVarIO (0 :: RpcId)
  sendLock <- newMVar ()

  let app conn = do
        putMVar connVar conn
        void (tryPutMVar readyVar (Right ()))
        readerLoop conn pending onNotif codec

  tid <- forkIO $ do
    result <- try (runClient ep codec app)
    case result of
      Left (e :: SomeException) ->
        void (tryPutMVar readyVar (Left (ConnectErr (HandshakeFailed (T.pack (show e))))))
      Right _ -> pure ()
    failAllPending pending

  ready <- takeMVar readyVar
  case ready of
    Left err -> throwIO err
    Right () -> pure ()
  conn <- readMVar connVar

  pure Engine
    { engRequest = sendRequest conn codec sendLock pending idRef
    , engCaps    = wsCaps
    , engClose   = closeConn tid conn
    }

sendRequest
  :: WS.Connection -> Codec -> MVar () -> Pending -> TVar RpcId
  -> MethodName -> [SurrealValue] -> IO SurrealValue
sendRequest conn codec sendLock pending idRef mname params = do
  rid  <- atomically $ do
            n <- readTVar idRef
            writeTVar idRef (n + 1)
            pure n
  slot <- newEmptyTMVarIO
  atomically $ modifyTVar' pending (HM.insert rid slot)
  let bytes = encodeValue codec (requestToValue (RpcRequest rid mname params))
  sendBytes conn codec bytes `catch` \(e :: SomeException) -> do
    atomically $ modifyTVar' pending (HM.delete rid)
    throwIO (TransportErr (WriteFailed (T.pack (show e))))
  res <- atomically (takeTMVar slot)
  either throwIO pure res
  where
    sendBytes c CodecCbor b = withMVar sendLock $ \_ -> WS.sendBinaryData c b
    sendBytes c CodecJson b = withMVar sendLock $ \_ -> WS.sendTextData c b

readerLoop :: WS.Connection -> Pending -> (Notification -> IO ()) -> Codec -> IO ()
readerLoop conn pending onNotif codec = go
  where
    go = do
      emsg <- try (WS.receiveDataMessage conn)
      case emsg of
        Left (_ :: SomeException) -> pure ()
        Right msg -> do
          dispatch (payload msg)
          go
    payload (WS.Binary b)  = b
    payload (WS.Text b _)  = b
    dispatch bytes =
      case decodeValue codec bytes of
        Left _  -> pure ()
        Right v -> case parseFrame v of
          Left _  -> pure ()
          Right (FrameResponse r)     -> deliver (resId r) (resResultEither r)
          Right (FrameNotification n) -> onNotif n
    resResultEither r = case resResult r of
      Left e  -> Left (RpcErr e)
      Right x -> Right x
    deliver rid result = do
      mslot <- atomically $ do
        m <- readTVar pending
        case HM.lookup rid m of
          Nothing   -> pure Nothing
          Just slot -> do
            writeTVar pending (HM.delete rid m)
            pure (Just slot)
      case mslot of
        Just slot -> atomically (void (tryPutTMVar slot result))
        Nothing   -> pure ()

failAllPending :: Pending -> IO ()
failAllPending pending = do
  slots <- atomically $ do
    m <- readTVar pending
    writeTVar pending HM.empty
    pure (HM.elems m)
  mapM_ (\slot -> atomically (void (tryPutTMVar slot (Left ConnectionLost)))) slots

runClient :: Endpoint -> Codec -> WS.ClientApp a -> IO a
runClient ep codec app
  | epSecure ep =
      WUSS.runSecureClientWith host (fromIntegral port) path WS.defaultConnectionOptions hdrs app
  | otherwise =
      WS.runClientWith host port path WS.defaultConnectionOptions hdrs app
  where
    host = T.unpack (epHost ep)
    port = epPort ep
    path = T.unpack (epPath ep)
    hdrs = [("Sec-WebSocket-Protocol", codecSubprotocol codec)]

closeConn :: ThreadId -> WS.Connection -> IO ()
closeConn tid conn = do
  (WS.sendClose conn ("bye" :: BL.ByteString)) `catch` \(_ :: SomeException) -> pure ()
  killThread tid
