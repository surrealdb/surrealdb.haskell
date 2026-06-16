{-# LANGUAGE OverloadedStrings #-}

-- | The core memory operations: remember, recall, context, reflect, forget and
-- the maintenance routines. Responses are returned as 'Data.Aeson.Value'.
module Spectron.Memory
  ( -- * Remember
    RememberOptions (..)
  , defaultRememberOptions
  , remember
  , RememberManyOptions (..)
  , defaultRememberManyOptions
  , rememberMany

    -- * Recall and friends
  , RecallOptions (..)
  , defaultRecallOptions
  , recall
  , context
  , reflect
  , forget

    -- * Maintenance
  , consolidate
  , elaborate
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson             (Value, toJSON)
import           Data.Text              (Text)

import           Spectron.Client
import           Spectron.Types

-- | Options for 'remember'.
data RememberOptions = RememberOptions
  { roMode     :: !(Maybe InferMode)
  , roCategory :: !(Maybe MemoryCategory)
  , roScope    :: !(Maybe Scope)
  , roMetadata :: !(Maybe Value)
  } deriving (Eq, Show)

-- | All fields unset.
defaultRememberOptions :: RememberOptions
defaultRememberOptions = RememberOptions Nothing Nothing Nothing Nothing

-- | Infer and store facts from free text.
remember :: MonadIO m => Spectron -> Text -> RememberOptions -> m Value
remember sp text opts =
  postJSON sp (contextPath sp "/facts") [] $ objectMaybe
    [ "text"     .=? Just text
    , "mode"     .=? fmap inferModeText (roMode opts)
    , "category" .=? fmap memoryCategoryText (roCategory opts)
    , "scope"    .=? fmap scopeToJSON (roScope opts)
    , "metadata" .=? roMetadata opts
    ]

-- | Options for 'rememberMany'.
data RememberManyOptions = RememberManyOptions
  { rmMode  :: !(Maybe BatchExtractionMode)
  , rmScope :: !(Maybe Scope)
  } deriving (Eq, Show)

-- | All fields unset.
defaultRememberManyOptions :: RememberManyOptions
defaultRememberManyOptions = RememberManyOptions Nothing Nothing

-- | Store facts inferred from a batch of conversation turns.
rememberMany :: MonadIO m => Spectron -> [BatchMessage] -> RememberManyOptions -> m Value
rememberMany sp messages opts =
  postJSON sp (contextPath sp "/facts/batch") [] $ objectMaybe
    [ "messages" .=? Just (toJSON messages)
    , "mode"     .=? fmap batchExtractionModeText (rmMode opts)
    , "scope"    .=? fmap scopeToJSON (rmScope opts)
    ]

-- | Options for 'recall'.
data RecallOptions = RecallOptions
  { reMode  :: !(Maybe QueryMode)
  , reK     :: !(Maybe Int)
  , reScope :: !(Maybe Scope)
  } deriving (Eq, Show)

-- | All fields unset.
defaultRecallOptions :: RecallOptions
defaultRecallOptions = RecallOptions Nothing Nothing Nothing

-- | Retrieve memories relevant to a query.
recall :: MonadIO m => Spectron -> Text -> RecallOptions -> m Value
recall sp query opts =
  postJSON sp (contextPath sp "/query") [] $ objectMaybe
    [ "query" .=? Just query
    , "mode"  .=? fmap queryModeText (reMode opts)
    , "k"     .=? reK opts
    , "scope" .=? fmap scopeToJSON (reScope opts)
    ]

-- | Assemble a context window for a query.
context :: MonadIO m => Spectron -> Text -> m Value
context sp query =
  postJSON sp (contextPath sp "/context") [] (objectMaybe ["query" .=? Just query])

-- | Reflect over memories for a query, optionally persisting the result.
reflect :: MonadIO m => Spectron -> Text -> Bool -> m Value
reflect sp query persist =
  postJSON sp (contextPath sp "/reflect") [] $ objectMaybe
    [ "query"   .=? Just query
    , "persist" .=? Just persist
    ]

-- | Forget memories matching a query.
forget :: MonadIO m => Spectron -> Text -> m Value
forget sp query =
  postJSON sp (contextPath sp "/forget") [] (objectMaybe ["query" .=? Just query])

-- | Run consolidation maintenance.
consolidate :: MonadIO m => Spectron -> m Value
consolidate sp = postJSON_ sp (contextPath sp "/consolidate") []

-- | Run elaboration maintenance.
elaborate :: MonadIO m => Spectron -> m Value
elaborate sp = postJSON_ sp (contextPath sp "/elaborate") []
