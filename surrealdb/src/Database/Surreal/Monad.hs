{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | The mtl friendly access to a connection. 'MonadSurreal' is intentionally a
-- one method class so it composes with any application monad. 'SurrealT' is the
-- batteries included runner.
module Database.Surreal.Monad
  ( MonadSurreal (..)
  , SurrealT (..)
  , runSurrealT
  , runSurreal
  ) where

import           Control.Monad.Catch       (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Reader
import qualified Control.Monad.State.Lazy   as SL
import qualified Control.Monad.State.Strict as SS
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Writer (WriterT)

import           Database.Surreal.Connection (Surreal)

-- | Any monad that can supply the active connection.
class MonadIO m => MonadSurreal m where
  askSurreal :: m Surreal

-- | A ReaderT over the connection handle. The default place to run SDK calls.
newtype SurrealT m a = SurrealT { unSurrealT :: ReaderT Surreal m a }
  deriving newtype
    ( Functor, Applicative, Monad, MonadIO, MonadTrans
    , MonadThrow, MonadCatch, MonadMask, MonadReader Surreal )

instance MonadIO m => MonadSurreal (SurrealT m) where
  askSurreal = SurrealT ask

-- | Run a 'SurrealT' action against a connection.
runSurrealT :: Surreal -> SurrealT m a -> m a
runSurrealT h = flip runReaderT h . unSurrealT

-- | Alias for 'runSurrealT' with the arguments in the order most callers want.
runSurreal :: Surreal -> SurrealT m a -> m a
runSurreal = runSurrealT

-- Pass-through instances so SDK methods drop into common transformer stacks.

instance MonadSurreal m => MonadSurreal (ReaderT r m) where
  askSurreal = lift askSurreal

instance MonadSurreal m => MonadSurreal (SS.StateT s m) where
  askSurreal = lift askSurreal

instance MonadSurreal m => MonadSurreal (SL.StateT s m) where
  askSurreal = lift askSurreal

instance MonadSurreal m => MonadSurreal (ExceptT e m) where
  askSurreal = lift askSurreal

instance (Monoid w, MonadSurreal m) => MonadSurreal (WriterT w m) where
  askSurreal = lift askSurreal
