{-# LANGUAGE OverloadedStrings #-}

-- | Status, profile and diagnostic endpoints.
module AgentMemory.Status
  ( health
  , state
  , profile
  , whoami
  , fsck
  , inspect
  , audit
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson             (Value)
import           Data.Text              (Text)

import           AgentMemory.Client

-- | Check that the service is healthy. Throws on a non success status.
health :: MonadIO m => AgentMemory -> m ()
health sp = requestNoContent sp "GET" (apiPath "/health") []

-- | The current memory state summary.
state :: MonadIO m => AgentMemory -> m Value
state sp = getJSON sp (contextPath sp "/state") []

-- | The context profile.
profile :: MonadIO m => AgentMemory -> m Value
profile sp = getJSON sp (contextPath sp "/profile") []

-- | Information about the calling principal.
whoami :: MonadIO m => AgentMemory -> m Value
whoami sp = getJSON sp (contextPath sp "/me") []

-- | Run a consistency check.
fsck :: MonadIO m => AgentMemory -> m Value
fsck sp = postJSON_ sp (contextPath sp "/fsck") []

-- | Inspect a reference such as a fact or entity id.
inspect :: MonadIO m => AgentMemory -> Text -> m Value
inspect sp ref = getJSON sp (contextPath sp "/inspect") [("ref", Just ref)]

-- | Retrieve the audit log.
audit :: MonadIO m => AgentMemory -> m Value
audit sp = getJSON sp (contextPath sp "/audit") []
