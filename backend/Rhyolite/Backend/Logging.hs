{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | typically useful config file:
--
--  [
--      { "logger":["Stderr",[]]
--      , "filters":
--          { "SQL":["Error",[]]}
--      } ,
--      { "logger": ["Journald",["<myAppName>"]]
--      , "filters": {"":["Warn",[]]}
--      }
--  ]
--
-- you could also add:
--     { "logger":["File",["sql.log"]}
--     , "filters":
--         { "":["Error",[]]
--         , "SQL":["Debug",[]]
--         }
--     } ,

module Rhyolite.Backend.Logging
  ( withLogging
  , LoggingEnv(..)
  , LoggingConfig(..)
  , runLoggingEnv
  , filterLog
  , LogAppender
  , getLogContext
  , RhyoliteLogAppender(..)
  , RhyoliteLogLevel(..)
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson hiding (Error)
import qualified Data.HashMap.Strict as HashMap
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Trie as Trie
import GHC.Generics
import System.Log.FastLogger
import Systemd.Journal

import Rhyolite.Request.TH (makeJson)

newtype LoggingEnv =  LoggingEnv { unLoggingEnv :: Loc -> LogSource -> LogLevel -> LogStr -> IO () }

data RhyoliteLogLevel
  = RhyoliteLogLevel_Debug
  | RhyoliteLogLevel_Info
  | RhyoliteLogLevel_Warn
  | RhyoliteLogLevel_Error
  deriving (Eq, Ord, Show, Generic, Enum)

makeJson ''RhyoliteLogLevel

toLogLevel :: RhyoliteLogLevel -> LogLevel
toLogLevel RhyoliteLogLevel_Debug = LevelDebug
toLogLevel RhyoliteLogLevel_Info = LevelInfo
toLogLevel RhyoliteLogLevel_Warn = LevelWarn
toLogLevel RhyoliteLogLevel_Error = LevelError

data LoggingConfig a = LoggingConfig
  { _loggingConfig_logger :: a
  , _loggingConfig_filters :: Map T.Text RhyoliteLogLevel
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

loggingConfigJsonOption :: Options
loggingConfigJsonOption = defaultOptions {fieldLabelModifier = drop ((length @ []) "_loggingConfig_")}

instance FromJSON a => FromJSON (LoggingConfig a) where
  parseJSON = genericParseJSON loggingConfigJsonOption

instance ToJSON a => ToJSON (LoggingConfig a) where
  toJSON = genericToJSON loggingConfigJsonOption
  toEncoding = genericToEncoding loggingConfigJsonOption


instance Semigroup LoggingEnv where
  LoggingEnv x <> LoggingEnv y = LoggingEnv $ x <> y

-- Bleh.  i can't say deriving newtype (Monoid) since i don't have DerivingStrateges yet.
instance Monoid LoggingEnv where
  mempty = LoggingEnv mempty

data LoggingContext m = LoggingContext
  { _loggingContext_cleanup :: m ()
  , _loggingContext_logger :: LoggingEnv
  }

data RhyoliteLogAppender
   = RhyoliteLogAppender_Stderr
   | RhyoliteLogAppender_File FilePath
   | RhyoliteLogAppender_Journald T.Text -- journalctl log with syslogIdentifier specified.
  deriving (Generic, Eq, Ord, Show)

makeJson ''RhyoliteLogAppender

runLoggingEnv :: LoggingEnv -> LoggingT m a -> m a
runLoggingEnv = flip runLoggingT . unLoggingEnv

logToFastLogger :: LoggerSet -> LoggingEnv
logToFastLogger ls = LoggingEnv $ \loc logSource logLevel logStr -> pushLogStrLn ls (toLogStr (show logLevel) <> toLogStr (show logSource) <> logStr)

logger2journald :: LogLevel -> JournalFields
logger2journald = \case
  LevelWarn -> priority Warning
  LevelDebug -> priority Debug
  LevelInfo -> priority Info
  LevelError -> priority Error
  LevelOther level -> priority Error  -- Error because that makes this logger2journald monotone
    <> mkJournalField' "PRIORITY_OTHER" level

class LogAppender a where
  getLogContext :: MonadIO m => a -> m (LoggingContext m)

instance LogAppender RhyoliteLogAppender where
  getLogContext = \case
      RhyoliteLogAppender_Stderr -> do
        logSet <- liftIO $ newStderrLoggerSet defaultBufSize
        return $ LoggingContext (liftIO (rmLoggerSet logSet)) (logToFastLogger logSet)
      RhyoliteLogAppender_File filename -> do
        logSet <- liftIO $ newFileLoggerSet defaultBufSize filename
        return $ LoggingContext (liftIO (rmLoggerSet logSet)) (logToFastLogger logSet)
      RhyoliteLogAppender_Journald syslogId -> return $ LoggingContext (return ()) (logToJournalCtl (syslogIdentifier syslogId))

configLogger :: (LogAppender a, MonadIO m) => LoggingConfig a -> m (LoggingContext m)
configLogger (LoggingConfig ls fs) = do
  LoggingContext cleaner logger <- getLogContext ls
  return $ LoggingContext cleaner $ filterLog (fmap toLogLevel fs) logger

mkJournalField' :: T.Text -> T.Text -> JournalFields
mkJournalField' k v = HashMap.singleton (mkJournalField k) (TE.encodeUtf8 v)

loc2jf :: Loc -> JournalFields
loc2jf (Loc fn pkg _ _ (line, _)) = mconcat
  [ codeFile fn
  , codeLine line
  , mkJournalField' "code_package" (T.pack pkg)
  ]

logToJournalCtl :: JournalFields -> LoggingEnv
logToJournalCtl syslogId = LoggingEnv $ \loc logSource logLevel logStr -> sendMessageWith (TE.decodeUtf8 $ fromLogStr logStr) $ mconcat
  [ syslogId
  , logger2journald logLevel
  , loc2jf loc
  , mkJournalField' "logsource" logSource
  ]

-- | match the LogSource on prefixes in m.  if found, log only the messages at equal or higher than the
--   matched level.  if not found, log only LevelWarn or higher.   You can override the default with a zero length prefix, ie `"" := LevelDebug`
filterLog :: Map T.Text LogLevel -> LoggingEnv -> LoggingEnv
filterLog m (LoggingEnv f) =
  let
    -- this gets checked a LOT.  so this needs to be hoisted out of the logging function.
    m' :: Trie.Trie LogLevel
    m' = Trie.fromList $ M.toList $ M.mapKeys TE.encodeUtf8 m
  in LoggingEnv $ \loc src lvl msg -> do
    let filterLvl = case Trie.match m' (TE.encodeUtf8 src) of
          Nothing -> LevelWarn
          Just (_, lvl', _) -> lvl'
    when (lvl >= filterLvl) $ do
      f loc src lvl msg


-- | handle `LoggingT` nicely.
withLogging :: (LogAppender l, MonadMask m, MonadIO m) => [LoggingConfig l] -> LoggingT m a -> m a
withLogging configs (LoggingT x) = do
  cleanersAndLoggers <- traverse configLogger configs
  let cleaner = foldl' (>>) (pure ())  $ fmap _loggingContext_cleanup cleanersAndLoggers
  let (LoggingEnv logger) = foldMap _loggingContext_logger cleanersAndLoggers
  finally (x logger) cleaner


