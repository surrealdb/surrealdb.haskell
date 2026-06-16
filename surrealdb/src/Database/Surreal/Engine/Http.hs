{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The stateless HTTP transport. Each RPC call is one POST to the @\/rpc@
-- endpoint. Because HTTP carries no session, the authentication token and the
-- selected namespace and database are attached as headers, read from the shared
-- session state that the connection layer keeps up to date.
module Database.Surreal.Engine.Http
  ( connectHttp
  ) where

import           Control.Exception       (SomeException, throwIO, try)
import qualified Data.ByteString.Lazy    as BL
import           Data.IORef              (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types      (statusCode)
import           Network.HTTP.Types.Header (RequestHeaders)

import           Database.Surreal.Codec
import           Database.Surreal.Engine
import           Database.Surreal.Error
import           Database.Surreal.RPC

-- | Open an HTTP engine against the endpoint. The 'IORef' holds the live
-- session state; the connection layer updates it as @use@, @signin@ and
-- @authenticate@ succeed.
connectHttp :: Endpoint -> Codec -> IORef SessionState -> IO Engine
connectHttp ep codec sessionRef = do
  manager <- newManager (if epSecure ep then tlsManagerSettings else defaultManagerSettings)
  baseReq <- parseRequest (T.unpack (baseUrl ep))
  idRef   <- newIORef (0 :: RpcId)
  let nextId = atomicModifyIORef' idRef (\n -> (n + 1, n))
      send mname params = do
        rid     <- nextId
        session <- readIORef sessionRef
        let body = encodeValue codec (requestToValue (RpcRequest rid mname params))
            req  = baseReq
              { method         = "POST"
              , requestBody    = RequestBodyLBS body
              , requestHeaders = headers codec session
              }
        eresp <- try (httpLbs req manager)
        case eresp of
          Left (e :: SomeException) ->
            throwIO (TransportErr (WriteFailed (T.pack (show e))))
          Right resp -> do
            let code = statusCode (responseStatus resp)
            if code >= 200 && code < 300
              then case decodeValue codec (responseBody resp) of
                     Left err -> throwIO err
                     Right v  -> case parseFrame v of
                       Right (FrameResponse r)     -> either (throwIO . RpcErr) pure (resResult r)
                       Right (FrameNotification _) ->
                         throwIO (ProtocolErr (UnexpectedFrame "notification on HTTP transport"))
                       Left pe                     -> throwIO (ProtocolErr pe)
              else throwIO (TransportErr (HttpStatus code (TE.decodeUtf8 (BL.toStrict (responseBody resp)))))
  pure Engine
    { engRequest = send
    , engCaps    = httpCaps
    , engClose   = pure ()
    }

baseUrl :: Endpoint -> T.Text
baseUrl ep =
  (if epSecure ep then "https://" else "http://")
    <> epHost ep <> ":" <> T.pack (show (epPort ep)) <> epPath ep

headers :: Codec -> SessionState -> RequestHeaders
headers codec session =
  [ ("Content-Type", codecContentType codec)
  , ("Accept",       codecContentType codec)
  ]
  ++ maybe [] (\t  -> [("Authorization", "Bearer " <> TE.encodeUtf8 t)]) (ssToken session)
  ++ maybe [] (\ns -> [("surreal-ns", TE.encodeUtf8 ns)]) (ssNamespace session)
  ++ maybe [] (\db -> [("surreal-db", TE.encodeUtf8 db)]) (ssDatabase session)
