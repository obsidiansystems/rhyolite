module Obelisk.Concurrency where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Control.Monad.IO.Class
import Control.Monad.Conc.Class
import Control.Concurrent.Classy.Async
import Control.Concurrent.Classy.STM.TVar
import Control.Monad
import Data.Fixed

withSingleWorker :: MonadConc m => Text -> m a -> m b -> m b
withSingleWorker threadName f g = do
  result <- race f g
  case result of
    Left _ -> error $ "Worker thread " <> T.unpack threadName <> " terminated unexpectedly"
    Right a -> pure a

threadDelayNDT :: MonadConc m => NominalDiffTime -> m ()
threadDelayNDT ndt = threadDelay (fromEnum (realToFrac ndt :: Micro))

withWatchdog :: forall m a. (MonadConc m, MonadIO m) => (Text -> IO ()) -> Text -> (m () -> m a) -> m a
withWatchdog logger threadName f = do
    currentTime <- liftIO getCurrentTime
    watchdogVar <- atomically $ newTVar currentTime
    let restartTimer :: m ()
        restartTimer = do
          current <- liftIO getCurrentTime
          atomically $ writeTVar watchdogVar current
        baseThreshold :: NominalDiffTime
        baseThreshold = 30
        watchdog currentThreshold = do
          threadDelayNDT baseThreshold
          lastRestarted <- readTVarConc watchdogVar
          currentTime <- liftIO getCurrentTime
          let lag = diffUTCTime currentTime lastRestarted
          if lag > currentThreshold
            then do
              liftIO $ logger $ "Watchdog: Thread " <> threadName <> " has not looped in more than " <> T.pack (show currentThreshold) <> " seconds"
              watchdog $ currentThreshold * 2
            else if lag < baseThreshold
                   then watchdog baseThreshold
                   else watchdog currentThreshold
    withSingleWorker (threadName <> "-watchdog") (watchdog baseThreshold) (f restartTimer)

withSingleWorkerWatchdog :: (MonadIO m, MonadConc m) => (Text -> IO ()) -> Text -> (m () -> m a) -> m b -> m b
-- Something is wrong with the watchdog timers, disable for the moment.
-- withSingleWorkerWatchdog logger threadName f g = withAsync (withWatchdog logger threadName f) $ \a -> link a >> g
-- withSingleWorkerWatchdog logger threadName f g = withSingleWorker threadName (f (pure ())) g
withSingleWorkerWatchdog logger threadName f g = withSingleWorker threadName (withWatchdog logger threadName f) g


