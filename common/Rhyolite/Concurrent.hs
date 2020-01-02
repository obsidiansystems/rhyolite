-- | Create worker threads that are respawned in case of error.

{-# LANGUAGE ScopedTypeVariables #-}

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
worker delay x = return . killThread <=< liftIO . forkIO . supervise . void . forever $
  x >> threadDelay delay

supervise :: Show a => IO a -> IO ()
supervise a = forever $ withAsync a $ \child -> do
  result <- waitCatch child
  printResult :: Either SomeException () <- try $ putStrLn $ "supervise: child terminated with " <> show result <> "; restarting"
  threadDelay 1000000
  when (isLeft printResult) $ putStrLn "supervise: note: an exception was encountered when printing the previous result"
