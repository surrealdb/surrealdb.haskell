{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Transactions. SurrealDB transactions are scoped to a connection, so the
-- bracket below sends @BEGIN TRANSACTION@, runs the action on the same
-- connection, then commits, cancelling on any exception. Transactions require
-- the WebSocket transport.
module Database.Surreal.Transaction
  ( withTransaction
  ) where

import           Control.Monad          (void)
import           Control.Monad.Catch    (MonadMask, onException)
import qualified Data.Map.Strict        as Map

import           Database.Surreal.Api          (query)
import           Database.Surreal.Connection   (requireCap, surrealCaps)
import           Database.Surreal.Engine        (capsTransaction)
import           Database.Surreal.Monad

-- | Run an action inside a SurrealDB transaction. Commits on success and
-- cancels if the action throws.
withTransaction :: (MonadSurreal m, MonadMask m) => m a -> m a
withTransaction act = do
  h <- askSurreal
  requireCap "transaction" (capsTransaction (surrealCaps h))
  void (query "BEGIN TRANSACTION;" Map.empty)
  result <- act `onException` cancel
  void (query "COMMIT TRANSACTION;" Map.empty)
  pure result
  where
    cancel = void (query "CANCEL TRANSACTION;" Map.empty)
