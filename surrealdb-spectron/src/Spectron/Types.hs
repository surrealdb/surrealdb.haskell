{-# LANGUAGE OverloadedStrings #-}

-- | Request types, enumerations and the scope selector for the Spectron client.
-- Response bodies are returned as 'Data.Aeson.Value' so the client tracks the
-- evolving Spectron API without locking callers to a fixed schema.
module Spectron.Types
  ( -- * Options
    SpectronOptions (..)
  , defaultSpectronOptions

    -- * Scope
  , Scope (..)
  , normaliseScope
  , scopeToJSON

    -- * Enumerations
  , InferMode (..)
  , inferModeText
  , MemoryCategory (..)
  , memoryCategoryText
  , TurnRole (..)
  , turnRoleText
  , QueryMode (..)
  , queryModeText
  , Verb (..)
  , verbText
  , BatchExtractionMode (..)
  , batchExtractionModeText

    -- * Messages
  , BatchMessage (..)

    -- * JSON helpers
  , (.=?)
  , objectMaybe
  ) where

import           Data.Aeson      (Key, ToJSON (..), Value (..), object)
import           Data.Aeson.Types (Pair)
import           Data.List       (nub)
import           Data.Text       (Text)

-- | Configuration for a Spectron client.
data SpectronOptions = SpectronOptions
  { soContext    :: !Text
    -- ^ The context id. Required.
  , soApiKey     :: !Text
    -- ^ The bearer token. Required.
  , soEndpoint   :: !Text
    -- ^ The base URL, for example @https:\/\/api.spectron.surrealdb.com@.
  , soTimeout    :: !Int
    -- ^ Per request timeout in microseconds.
  , soMaxRetries :: !Int
    -- ^ Maximum retries for idempotent requests.
  } deriving (Eq, Show)

-- | Defaults: a thirty second timeout and three retries. The context, api key
-- and endpoint must be filled in.
defaultSpectronOptions :: Text -> Text -> Text -> SpectronOptions
defaultSpectronOptions ctx key endpoint = SpectronOptions
  { soContext    = ctx
  , soApiKey     = key
  , soEndpoint   = endpoint
  , soTimeout    = 30 * 1000000
  , soMaxRetries = 3
  }

-- | A scope selector in disjunctive normal form: an outer OR of inner ANDs.
newtype Scope = Scope { unScope :: [[Text]] }
  deriving (Eq, Show)

-- | Normalise a scope by dropping empty terms and clauses and removing
-- duplicates while preserving order.
normaliseScope :: Scope -> Scope
normaliseScope (Scope clauses) =
  Scope (nub (filter (not . null) (map (nub . filter (/= "")) clauses)))

-- | Render a normalised scope as a JSON array of arrays.
scopeToJSON :: Scope -> Value
scopeToJSON = toJSON . unScope . normaliseScope

-- | How facts are inferred from input.
data InferMode = InferFull | InferTriples | InferPreview | InferNone
  deriving (Eq, Show)

inferModeText :: InferMode -> Text
inferModeText InferFull    = "full"
inferModeText InferTriples = "triples"
inferModeText InferPreview = "preview"
inferModeText InferNone    = "none"

-- | The memory category a fact belongs to.
data MemoryCategory = CategoryIdentity | CategoryKnowledge | CategoryContext
  deriving (Eq, Show)

memoryCategoryText :: MemoryCategory -> Text
memoryCategoryText CategoryIdentity  = "identity"
memoryCategoryText CategoryKnowledge = "knowledge"
memoryCategoryText CategoryContext   = "context"

-- | The role of a conversation turn.
data TurnRole = RoleUser | RoleAssistant | RoleSystem | RoleTool
  deriving (Eq, Show)

turnRoleText :: TurnRole -> Text
turnRoleText RoleUser      = "user"
turnRoleText RoleAssistant = "assistant"
turnRoleText RoleSystem    = "system"
turnRoleText RoleTool      = "tool"

-- | The retrieval strategy for a recall.
data QueryMode = QueryHybrid | QueryVector | QueryBm25 | QueryHybridGraph
  deriving (Eq, Show)

queryModeText :: QueryMode -> Text
queryModeText QueryHybrid      = "hybrid"
queryModeText QueryVector      = "vector"
queryModeText QueryBm25        = "bm25"
queryModeText QueryHybridGraph = "hybrid_graph"

-- | A grant verb.
data Verb = VerbRead | VerbWrite | VerbCreateScope | VerbDeleteScope | VerbGrant | VerbManage | VerbForget
  deriving (Eq, Show)

verbText :: Verb -> Text
verbText VerbRead        = "read"
verbText VerbWrite       = "write"
verbText VerbCreateScope = "create_scope"
verbText VerbDeleteScope = "delete_scope"
verbText VerbGrant       = "grant"
verbText VerbManage      = "manage"
verbText VerbForget      = "forget"

-- | How a batch of messages is extracted.
data BatchExtractionMode = PerMessage | WholeConversation
  deriving (Eq, Show)

batchExtractionModeText :: BatchExtractionMode -> Text
batchExtractionModeText PerMessage        = "per_message"
batchExtractionModeText WholeConversation = "whole_conversation"

-- | One message in a 'rememberMany' batch.
data BatchMessage = BatchMessage
  { bmRole    :: !TurnRole
  , bmContent :: !Text
  } deriving (Eq, Show)

instance ToJSON BatchMessage where
  toJSON (BatchMessage role content) =
    object [ "role" `pair` String (turnRoleText role)
           , "content" `pair` String content ]
    where pair = (,)

-- | A pair that is only included when the value is present.
(.=?) :: ToJSON v => Key -> Maybe v -> Maybe Pair
k .=? mv = fmap (\v -> (k, toJSON v)) mv
infixr 8 .=?

-- | Build an object, omitting absent fields.
objectMaybe :: [Maybe Pair] -> Value
objectMaybe = object . concatMap (maybe [] pure)
