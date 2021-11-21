-- | Create worker threads that are respawned in case of error.

{-# Language ScopedTypeVariables #-}

module Rhyolite.Concurrent where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.Async (withAsync, waitCatch)
import Control.Exception (SomeException, try)
import Control.Monad (forever, void, when, (<=<))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Semigroup ((<>))
import Data.Either (isLeft)


-- | Perform a supervised operation with delays in between, on a separate thread. Return an IO action for killing the thread.
worker :: (MonadIO m)
       => Int -- ^ Delay between operations, in microseconds
       -> IO a -- ^ Operation to perform
       -> m (IO ())
worker = taggedWorker ""

-- | Perform a tagged, supervised operation with delays in between, on a separate thread. Return an IO action for killing the thread.
taggedWorker
  :: MonadIO m
  => String -- ^ Tag for this worker: displayed in error messages
  -> Int
  -> IO a
  -> m (IO ())
taggedWorker tag delay x = return . killThread <=< liftIO . forkIO . supervise tag . void . forever $
  x >> threadDelay delay

supervise :: Show a => String -> IO a -> IO ()
supervise tag a = forever $ withAsync a $ \child -> do
  let msgPrefix = if null tag then "supervise: " else "supervise: " <> tag <> ": "
  result <- waitCatch child
  printResult :: Either SomeException () <- try $ putStrLn $ msgPrefix <> "child terminated with " <> show result <> "; restarting"
  threadDelay 1000000
  when (isLeft printResult) $ putStrLn $ msgPrefix <> "note: an exception was encountered when printing the previous result"
