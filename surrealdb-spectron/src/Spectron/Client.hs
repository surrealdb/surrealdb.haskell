{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | The Spectron client handle and the request machinery every method shares:
-- bearer auth, the optional on behalf of delegation header, timeouts, retries
-- for idempotent requests and problem response decoding.
module Spectron.Client
  ( Spectron (..)
  , newSpectron
  , onBehalfOf
  , contextPath
  , apiPath

    -- * Request runners
  , getJSON
  , postJSON
  , postJSON_
  , deleteJSON
  , requestNoContent
  , requestJSON
  , rawRequest
  ) where

import           Control.Concurrent       (threadDelay)
import           Control.Exception        (SomeException, throwIO, try)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Aeson               (Value (..), eitherDecode, encode)
import qualified Data.Aeson.KeyMap        as KM
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BL
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Network.HTTP.Types       (statusCode)
import           Network.HTTP.Types.Header (HeaderName)

import           Spectron.Error
import           Spectron.Types

-- | A configured Spectron client.
data Spectron = Spectron
  { spOptions :: !SpectronOptions
  , spManager :: !Manager
  , spObo     :: !(Maybe Text)
  }

-- | Build a client. Creates a TLS capable connection manager.
newSpectron :: MonadIO m => SpectronOptions -> m Spectron
newSpectron opts = liftIO $ do
  manager <- newManager tlsManagerSettings
  pure (Spectron opts { soEndpoint = stripTrailing (soEndpoint opts) } manager Nothing)
  where
    stripTrailing = T.dropWhileEnd (== '/')

-- | Return a client that acts on behalf of the given principal. Adds the
-- delegation header to every request.
onBehalfOf :: Spectron -> Text -> Spectron
onBehalfOf sp principal = sp { spObo = Just principal }

-- | A path under the context prefix @\/api\/v1\/{context}@.
contextPath :: Spectron -> Text -> Text
contextPath sp seg = "/api/v1/" <> soContext (spOptions sp) <> seg

-- | A path under the bare @\/api\/v1@ prefix, for context free endpoints.
apiPath :: Text -> Text
apiPath seg = "/api/v1" <> seg

-- Request runners ------------------------------------------------------------

-- | Perform a GET and decode the JSON body. Retried on server and connection
-- failures because GET is idempotent.
getJSON :: MonadIO m => Spectron -> Text -> [(Text, Maybe Text)] -> m Value
getJSON sp path qs = liftIO (run sp True "GET" path qs Nothing)

-- | Perform a POST with a JSON body and decode the JSON response.
postJSON :: MonadIO m => Spectron -> Text -> [(Text, Maybe Text)] -> Value -> m Value
postJSON sp path qs body = liftIO (run sp False "POST" path qs (Just body))

-- | Perform a POST with no body and decode the JSON response.
postJSON_ :: MonadIO m => Spectron -> Text -> [(Text, Maybe Text)] -> m Value
postJSON_ sp path qs = liftIO (run sp False "POST" path qs (Just (Object KM.empty)))

-- | Perform a DELETE and decode any JSON response (or 'Null' on 204).
deleteJSON :: MonadIO m => Spectron -> Text -> [(Text, Maybe Text)] -> m Value
deleteJSON sp path qs = liftIO (run sp False "DELETE" path qs Nothing)

-- | Perform a request that returns no content.
requestNoContent :: MonadIO m => Spectron -> BS.ByteString -> Text -> [(Text, Maybe Text)] -> m ()
requestNoContent sp method path qs =
  liftIO (() <$ run sp (method == "GET") method path qs Nothing)

-- | Perform a request with an explicit method and optional body, decoding the
-- JSON response. GET requests are retried.
requestJSON :: MonadIO m => Spectron -> BS.ByteString -> Text -> [(Text, Maybe Text)] -> Maybe Value -> m Value
requestJSON sp method path qs body = liftIO (run sp (method == "GET") method path qs body)

-- Internal -------------------------------------------------------------------

run :: Spectron -> Bool -> BS.ByteString -> Text -> [(Text, Maybe Text)] -> Maybe Value -> IO Value
run sp idempotent method path qs mbody = go 0
  where
    opts     = spOptions sp
    maxTries = if idempotent then max 1 (soMaxRetries opts + 1) else 1

    go attempt = do
      result <- attemptOnce
      case result of
        Right v -> pure v
        Left err
          | retriable (seKind err) && attempt + 1 < maxTries -> do
              threadDelay (backoff attempt)
              go (attempt + 1)
          | otherwise -> throwIO err

    attemptOnce :: IO (Either SpectronError Value)
    attemptOnce = do
      let url = T.unpack (soEndpoint opts <> path)
      ereq <- try (parseRequest url) :: IO (Either SomeException Request)
      case ereq of
        Left e -> pure (Left (connErr (T.pack (show e))))
        Right base -> do
          let req = setQueryString (queryBytes qs) base
                { method          = method
                , requestHeaders  = buildHeaders sp method
                , requestBody     = maybe (RequestBodyLBS BL.empty) (RequestBodyLBS . encode) mbody
                , responseTimeout = responseTimeoutMicro (soTimeout opts)
                }
          eresp <- try (httpLbs req (spManager sp)) :: IO (Either SomeException (Response BL.ByteString))
          case eresp of
            Left e     -> pure (Left (connErr (T.pack (show e))))
            Right resp -> pure (interpret resp)

    interpret resp =
      let code = statusCode (responseStatus resp)
          body = responseBody resp
      in if code >= 200 && code < 300
           then if BL.null body
                  then Right Null
                  else case eitherDecode body of
                         Right v -> Right v
                         Left e  -> Left (SpectronError code ConnectionFailed "invalid JSON response" (Just (T.pack e)))
           else Left (problemError code body)

    retriable ServerFailed     = True
    retriable ConnectionFailed = True
    retriable _                = False

    backoff attempt = 100000 * (2 ^ attempt)  -- 100ms, 200ms, 400ms, ...

    connErr msg = SpectronError 0 ConnectionFailed "connection failed" (Just msg)

-- | Perform a single request and return the raw response body, throwing on a
-- non success status. Used for streaming endpoints, which are not retried.
rawRequest :: MonadIO m => Spectron -> BS.ByteString -> Text -> [(Text, Maybe Text)] -> Maybe Value -> m BL.ByteString
rawRequest sp method path qs mbody = liftIO $ do
  let opts = spOptions sp
      url  = T.unpack (soEndpoint opts <> path)
  ereq <- try (parseRequest url) :: IO (Either SomeException Request)
  case ereq of
    Left e -> throwIO (SpectronError 0 ConnectionFailed "connection failed" (Just (T.pack (show e))))
    Right base -> do
      let req = setQueryString (queryBytes qs) base
            { method          = method
            , requestHeaders  = buildHeaders sp method
            , requestBody     = maybe (RequestBodyLBS BL.empty) (RequestBodyLBS . encode) mbody
            , responseTimeout = responseTimeoutMicro (soTimeout opts)
            }
      eresp <- try (httpLbs req (spManager sp)) :: IO (Either SomeException (Response BL.ByteString))
      case eresp of
        Left e -> throwIO (SpectronError 0 ConnectionFailed "connection failed" (Just (T.pack (show e))))
        Right resp ->
          let code = statusCode (responseStatus resp)
          in if code >= 200 && code < 300
               then pure (responseBody resp)
               else throwIO (problemError code (responseBody resp))

problemError :: Int -> BL.ByteString -> SpectronError
problemError code body =
  let kind = classifyStatus code
      (title, detail) = case eitherDecode body of
        Right (Object o) ->
          ( textField o "title" (defaultTitle kind)
          , textFieldMaybe o "detail" )
        _ -> (defaultTitle kind, if BL.null body then Nothing else Just (TE.decodeUtf8 (BL.toStrict body)))
  in SpectronError code kind title detail
  where
    textField o k def = case KM.lookup k o of
                          Just (String s) -> s
                          _               -> def
    textFieldMaybe o k = case KM.lookup k o of
                           Just (String s) -> Just s
                           _               -> Nothing
    defaultTitle k = T.pack (show k)

buildHeaders :: Spectron -> BS.ByteString -> [(HeaderName, BS.ByteString)]
buildHeaders sp method =
  [ ("Authorization", "Bearer " <> TE.encodeUtf8 (soApiKey (spOptions sp)))
  , ("User-Agent",    "surrealdb-spectron-haskell/0.1.0")
  , ("Accept",        "application/json")
  ]
  ++ [ ("Content-Type", "application/json") | method /= "GET" && method /= "DELETE" ]
  ++ maybe [] (\p -> [("X-Spectron-On-Behalf-Of", TE.encodeUtf8 p)]) (spObo sp)

queryBytes :: [(Text, Maybe Text)] -> [(BS.ByteString, Maybe BS.ByteString)]
queryBytes = map (\(k, mv) -> (TE.encodeUtf8 k, fmap TE.encodeUtf8 mv))
