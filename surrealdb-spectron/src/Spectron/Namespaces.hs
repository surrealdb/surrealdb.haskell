{-# LANGUAGE OverloadedStrings #-}

-- | The Spectron resource namespaces: documents, entities, sessions,
-- lifecycle, traces, principals, scopes and keys. JavaScript groups these on
-- sub objects; here they are plain functions prefixed by their namespace.
-- Responses are 'Data.Aeson.Value'.
module Spectron.Namespaces
  ( -- * Documents
    documentsList
  , documentsGet
  , documentsRaw
  , documentsChunks
  , documentsDelete
  , documentsQuery
  , documentsUpload

    -- * Entities
  , entitiesList
  , entitiesGet
  , entitiesHistory
  , entitiesDelete

    -- * Sessions
  , sessionsCreate
  , sessionClose
  , sessionTurns
  , sessionContext

    -- * Lifecycle
  , lifecycleExpire
  , lifecycleDecay

    -- * Traces
  , tracesList
  , tracesGet
  , tracesStats

    -- * Principals
  , principalsList
  , principalsGet
  , principalsEffective
  , principalsGrant
  , principalsRevoke

    -- * Scopes
  , scopesList
  , scopesRegister
  , scopesDelete
  , scopesForget

    -- * Keys
  , keysCreate
  , keysList
  , keysDelete
  , keysRotate
  ) where

import           Control.Exception      (throwIO)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (Value (..), eitherDecode, object, (.=))
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           Network.HTTP.Client    hiding (path)
import           Network.HTTP.Client.MultipartFormData (formDataBody, partBS, partContentType, partFileRequestBody)
import           Network.HTTP.Types     (statusCode, urlEncode)
import           Network.HTTP.Types.Header (HeaderName)

import           Spectron.Client
import           Spectron.Error
import           Spectron.Types

-- Documents ------------------------------------------------------------------

-- | List documents, with optional filter query parameters such as @status@ or
-- @mimeType@ and pagination via @page@ and @pageSize@.
documentsList :: MonadIO m => Spectron -> [(Text, Maybe Text)] -> m Value
documentsList sp filters = getJSON sp (contextPath sp "/documents") filters

-- | Fetch a document's metadata.
documentsGet :: MonadIO m => Spectron -> Text -> m Value
documentsGet sp docId = getJSON sp (contextPath sp ("/documents/" <> seg docId)) []

-- | Fetch a document's raw bytes.
documentsRaw :: MonadIO m => Spectron -> Text -> m BL.ByteString
documentsRaw sp docId = rawRequest sp "GET" (contextPath sp ("/documents/" <> seg docId <> "/raw")) [] Nothing

-- | Fetch a page of a document's chunks.
documentsChunks :: MonadIO m => Spectron -> Text -> Maybe Int -> Maybe Int -> m Value
documentsChunks sp docId page pageSize =
  getJSON sp (contextPath sp ("/documents/" <> seg docId <> "/chunks"))
    (catQuery [("page", page), ("pageSize", pageSize)])

-- | Delete a document.
documentsDelete :: MonadIO m => Spectron -> Text -> m ()
documentsDelete sp docId = requestNoContent sp "DELETE" (contextPath sp ("/documents/" <> seg docId)) []

-- | Run a structured document query.
documentsQuery :: MonadIO m => Spectron -> Value -> m Value
documentsQuery sp body = postJSON sp (contextPath sp "/documents/query") [] body

-- | Upload a document as multipart form data.
documentsUpload :: MonadIO m => Spectron -> Text -> BS.ByteString -> BS.ByteString -> m Value
documentsUpload sp filename mimeType bytes = liftIO $ do
  let opts = spOptions sp
      url  = T.unpack (soEndpoint opts <> contextPath sp "/documents")
  base <- parseRequest url
  let part = (partFileRequestBody "file" (T.unpack filename) (RequestBodyBS bytes))
               { partContentType = Just mimeType }
  req0 <- formDataBody [part, partBS "filename" (TE.encodeUtf8 filename)] base
  let req = req0
        { method          = "POST"
        , requestHeaders  = authHeaders sp ++ filter (isContentType . fst) (requestHeaders req0)
        , responseTimeout = responseTimeoutMicro (soTimeout opts)
        }
  resp <- httpLbs req (spManager sp)
  let code = statusCode (responseStatus resp)
  if code >= 200 && code < 300
    then pure (decodeValueOrNull (responseBody resp))
    else throwSpectron code (responseBody resp)
  where isContentType h = h == "Content-Type"

-- Entities -------------------------------------------------------------------

-- | List entities, optionally filtered by type.
entitiesList :: MonadIO m => Spectron -> Maybe Text -> m Value
entitiesList sp mtype = getJSON sp (contextPath sp "/entities") (catQueryText [("type", mtype)])

-- | Fetch an entity by type and name.
entitiesGet :: MonadIO m => Spectron -> Text -> Text -> m Value
entitiesGet sp ty name = getJSON sp (contextPath sp ("/entities/" <> seg ty <> "/" <> seg name)) []

-- | Fetch the history of an entity attribute.
entitiesHistory :: MonadIO m => Spectron -> Text -> Text -> Text -> m Value
entitiesHistory sp ty name key =
  getJSON sp (contextPath sp ("/entities/" <> seg ty <> "/" <> seg name <> "/history/" <> seg key)) []

-- | Delete an entity.
entitiesDelete :: MonadIO m => Spectron -> Text -> Text -> m ()
entitiesDelete sp ty name = requestNoContent sp "DELETE" (contextPath sp ("/entities/" <> seg ty <> "/" <> seg name)) []

-- Sessions -------------------------------------------------------------------

-- | Create a session, optionally with a scope and metadata.
sessionsCreate :: MonadIO m => Spectron -> Maybe Scope -> Maybe Value -> m Value
sessionsCreate sp mscope mmeta =
  postJSON sp (contextPath sp "/sessions") [] $ objectMaybe
    [ "scope"    .=? fmap scopeToJSON mscope
    , "metadata" .=? mmeta
    ]

-- | Close a session.
sessionClose :: MonadIO m => Spectron -> Text -> m ()
sessionClose sp sid = requestNoContent sp "DELETE" (contextPath sp ("/sessions/" <> seg sid)) []

-- | List the turns of a session.
sessionTurns :: MonadIO m => Spectron -> Text -> m Value
sessionTurns sp sid = getJSON sp (contextPath sp ("/sessions/" <> seg sid <> "/turns")) []

-- | Build a context window scoped to a session.
sessionContext :: MonadIO m => Spectron -> Text -> Text -> m Value
sessionContext sp sid query =
  postJSON sp (contextPath sp ("/sessions/" <> seg sid <> "/context")) [] (object ["query" .= query])

-- Lifecycle ------------------------------------------------------------------

-- | Expire memories whose lifetime has elapsed.
lifecycleExpire :: MonadIO m => Spectron -> m Value
lifecycleExpire sp = postJSON_ sp (contextPath sp "/lifecycle/expire") []

-- | Apply decay to memory strengths.
lifecycleDecay :: MonadIO m => Spectron -> m Value
lifecycleDecay sp = postJSON_ sp (contextPath sp "/lifecycle/decay") []

-- Traces ---------------------------------------------------------------------

-- | List retrieval traces, optionally limited.
tracesList :: MonadIO m => Spectron -> Maybe Int -> m Value
tracesList sp mlimit = getJSON sp (contextPath sp "/traces") (catQuery [("limit", mlimit)])

-- | Fetch a trace by id.
tracesGet :: MonadIO m => Spectron -> Text -> m Value
tracesGet sp tid = getJSON sp (contextPath sp ("/traces/" <> seg tid)) []

-- | Trace statistics.
tracesStats :: MonadIO m => Spectron -> m Value
tracesStats sp = getJSON sp (contextPath sp "/traces/stats") []

-- Principals -----------------------------------------------------------------

-- | List principals. Requires the manage grant.
principalsList :: MonadIO m => Spectron -> m Value
principalsList sp = getJSON sp (contextPath sp "/principals") []

-- | Fetch a principal.
principalsGet :: MonadIO m => Spectron -> Text -> m Value
principalsGet sp pid = getJSON sp (contextPath sp ("/principals/" <> seg pid)) []

-- | Compute the effective grants of a principal at a path.
principalsEffective :: MonadIO m => Spectron -> Text -> Text -> Maybe Text -> m Value
principalsEffective sp pid path asOf =
  getJSON sp (contextPath sp ("/principals/" <> seg pid <> "/effective"))
    ([("path", Just path)] ++ catQueryText [("asOf", asOf)])

-- | Grant verbs on a path to a principal.
principalsGrant :: MonadIO m => Spectron -> Text -> Text -> [Verb] -> m Value
principalsGrant sp pid path verbs =
  postJSON sp (contextPath sp ("/principals/" <> seg pid <> "/grants")) [] (grantBody path verbs)

-- | Revoke verbs on a path from a principal.
principalsRevoke :: MonadIO m => Spectron -> Text -> Text -> [Verb] -> m Value
principalsRevoke sp pid path verbs =
  requestJSON sp "DELETE" (contextPath sp ("/principals/" <> seg pid <> "/grants")) [] (Just (grantBody path verbs))

grantBody :: Text -> [Verb] -> Value
grantBody path verbs = object ["path" .= path, "verbs" .= map verbText verbs]

-- Scopes ---------------------------------------------------------------------

-- | List the scope tree.
scopesList :: MonadIO m => Spectron -> m Value
scopesList sp = getJSON sp (contextPath sp "/scopes") []

-- | Register a scope.
scopesRegister :: MonadIO m => Spectron -> Text -> Maybe Text -> Maybe Text -> m Value
scopesRegister sp path displayName description =
  postJSON sp (contextPath sp "/scopes") [] $ objectMaybe
    [ "path"        .=? Just path
    , "displayName" .=? displayName
    , "description" .=? description
    ]

-- | Delete a scope by path.
scopesDelete :: MonadIO m => Spectron -> Text -> m ()
scopesDelete sp path = requestNoContent sp "DELETE" (contextPath sp "/scopes") [("path", Just path)]

-- | Forget all memories under a scope path.
scopesForget :: MonadIO m => Spectron -> Maybe Text -> m Value
scopesForget sp mpath =
  postJSON sp (contextPath sp "/scopes/forget") [] (objectMaybe ["path" .=? mpath])

-- Keys -----------------------------------------------------------------------

-- | Mint a new API key.
keysCreate :: MonadIO m => Spectron -> Maybe Text -> Maybe Value -> Maybe Int -> m Value
keysCreate sp name grants ttl =
  postJSON sp (contextPath sp "/keys") (catQuery [("ttlSeconds", ttl)]) $ objectMaybe
    [ "name"   .=? name
    , "grants" .=? grants
    ]

-- | List API keys.
keysList :: MonadIO m => Spectron -> m Value
keysList sp = getJSON sp (contextPath sp "/keys") []

-- | Delete an API key.
keysDelete :: MonadIO m => Spectron -> Text -> m ()
keysDelete sp keyName = requestNoContent sp "DELETE" (contextPath sp ("/keys/" <> seg keyName)) []

-- | Rotate an API key.
keysRotate :: MonadIO m => Spectron -> Text -> Maybe Int -> m Value
keysRotate sp keyName ttl =
  postJSON_ sp (contextPath sp ("/keys/" <> seg keyName <> "/rotate")) (catQuery [("ttlSeconds", ttl)])

-- Helpers --------------------------------------------------------------------

-- | Percent encode a path segment.
seg :: Text -> Text
seg = TE.decodeUtf8 . urlEncode False . TE.encodeUtf8

catQuery :: [(Text, Maybe Int)] -> [(Text, Maybe Text)]
catQuery = concatMap (\(k, mv) -> maybe [] (\v -> [(k, Just (T.pack (show v)))]) mv)

catQueryText :: [(Text, Maybe Text)] -> [(Text, Maybe Text)]
catQueryText = concatMap (\(k, mv) -> maybe [] (\v -> [(k, Just v)]) mv)

authHeaders :: Spectron -> [(HeaderName, BS.ByteString)]
authHeaders sp =
  [ ("Authorization", "Bearer " <> TE.encodeUtf8 (soApiKey (spOptions sp)))
  , ("User-Agent",    "surrealdb-spectron-haskell/0.1.0")
  , ("Accept",        "application/json")
  ]
  ++ maybe [] (\p -> [("X-Spectron-On-Behalf-Of", TE.encodeUtf8 p)]) (spObo sp)

decodeValueOrNull :: BL.ByteString -> Value
decodeValueOrNull body
  | BL.null body = Null
  | otherwise    = either (const Null) id (eitherDecode body)

throwSpectron :: Int -> BL.ByteString -> IO a
throwSpectron code body =
  throwIO (SpectronError code (classifyStatus code) "upload failed" (Just (TE.decodeUtf8 (BL.toStrict body))))
