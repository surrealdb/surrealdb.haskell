{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Live queries. A live query subscribes to changes on a table and yields
-- notifications as they arrive. Live queries require the WebSocket transport;
-- calling 'live' over HTTP throws 'UnsupportedByTransport'.
module Database.Surreal.Live
  ( LiveQuery (..)
  , live
  , nextNotification
  , tryNextNotification
  , kill
  , Notification (..)
  , Action (..)
  ) where

import           Control.Concurrent.STM
import           Control.Exception      (throwIO)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text              (Text)

import           Database.Surreal.Connection
import           Database.Surreal.Engine     (capsLive)
import           Database.Surreal.Error
import           Database.Surreal.Monad
import           Database.Surreal.RPC        (Action (..), Notification (..))
import           Database.Surreal.Types

-- | A handle to an active live query.
data LiveQuery = LiveQuery
  { lqId    :: !Uuid
  , lqQueue :: !(TQueue Notification)
  , lqConn  :: !Surreal
  }

-- | Start a live query on a table and return its handle.
live :: MonadSurreal m => Text -> m LiveQuery
live tbl = do
  h <- askSurreal
  requireCap "live" (capsLive (surrealCaps h))
  res <- invoke h "live" [VTable (Table tbl)]
  qid <- liftIO $ case res of
    VUuid u   -> pure u
    VString s -> maybe (throwIO (ProtocolErr (UnexpectedFrame "live returned a bad uuid"))) pure (uuidFromText s)
    _         -> throwIO (ProtocolErr (UnexpectedFrame "live did not return a uuid"))
  q <- liftIO (registerLive h qid)
  pure (LiveQuery qid q h)

-- | Block until the next notification arrives.
nextNotification :: MonadIO m => LiveQuery -> m Notification
nextNotification lq = liftIO (atomically (readTQueue (lqQueue lq)))

-- | Return the next notification if one is already buffered.
tryNextNotification :: MonadIO m => LiveQuery -> m (Maybe Notification)
tryNextNotification lq = liftIO (atomically (tryReadTQueue (lqQueue lq)))

-- | Stop a live query and release its subscription.
kill :: MonadSurreal m => LiveQuery -> m ()
kill lq = do
  h <- askSurreal
  invokeUnit h "kill" [VUuid (lqId lq)]
  liftIO (unregisterLive h (lqId lq))
