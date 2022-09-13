module Control.Monad.MVar.Restricted
  ( MonadMVar (..)
  ) where

import Data.Constraint
import Data.Constraint.Empty
import GHC.Stack
import qualified Control.Concurrent
import Control.Monad.Reader
import Control.Monad.Catch (MonadMask(..), onException)

class Monad m => MonadMVar m where
  type MVarData m :: * -> Constraint
  type MVar m :: * -> *
  newEmptyMVar :: (HasCallStack, MVarData m a) => m (MVar m a)
  newMVar :: (HasCallStack, MVarData m a) => a -> m (MVar m a)
  putMVar :: MVarData m a => MVar m a -> a -> m ()
  takeMVar :: MVarData m a => MVar m a -> m a
  readMVar :: MVarData m a => MVar m a -> m a
  withMVar :: MVarData m a => MVar m a -> (a -> m b) -> m b
  default withMVar :: (MonadMask m, MVarData m a) => MVar m a -> (a -> m b) -> m b
  withMVar m io =
    mask $ \restore -> do
      a <- takeMVar m
      b <- restore (io a) `onException` putMVar m a
      putMVar m a
      return b
  modifyMVar_ :: MVarData m a => MVar m a -> (a -> m a) -> m ()
  modifyMVar_ v f = modifyMVar v $ \x -> (,) <$> f x <*> pure ()
  modifyMVar :: MVarData m a => MVar m a -> (a -> m (a, b)) -> m b
  default modifyMVar :: (MonadMask m, MVarData m a) => MVar m a -> (a -> m (a, b)) -> m b
  modifyMVar m io =
    mask $ \restore -> do
      a      <- takeMVar m
      (a',b) <- restore (io a) `onException` putMVar m a -- TODO: IO puts an evaluate after the action to restore the old state even when unpacking the resulting tuple.  This is probably desirable, but not really expressible without dirty tricks.
      putMVar m a'
      return b
instance MonadMVar IO where
  type MVarData IO = EmptyConstraint
  type MVar IO = Control.Concurrent.MVar
  newEmptyMVar = Control.Concurrent.newEmptyMVar
  newMVar = Control.Concurrent.newMVar
  putMVar = Control.Concurrent.putMVar
  takeMVar = Control.Concurrent.takeMVar
  readMVar = Control.Concurrent.readMVar
  withMVar = Control.Concurrent.withMVar
  modifyMVar_ = Control.Concurrent.modifyMVar_
  modifyMVar = Control.Concurrent.modifyMVar

instance MonadMVar m => MonadMVar (ReaderT r m) where
  type MVarData (ReaderT r m) = MVarData m
  type MVar (ReaderT r m) = MVar m
  newEmptyMVar = lift newEmptyMVar
  newMVar = lift . newMVar
  putMVar v = lift . putMVar v
  takeMVar = lift . takeMVar
  readMVar = lift . readMVar
  withMVar v a = do
    r <- ask
    lift $ withMVar v $ \x -> runReaderT (a x) r
  modifyMVar_ v a = do
    r <- ask
    lift $ modifyMVar_ v $ \x -> runReaderT (a x) r
  modifyMVar v a = do
    r <- ask
    lift $ modifyMVar v $ \x -> runReaderT (a x) r
