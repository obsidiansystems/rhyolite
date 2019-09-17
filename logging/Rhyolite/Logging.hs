{-# LANGUAGE CPP #-}
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

-- | Typically useful config file, which logs everything to stderr and warnings to journald.
--
-- [
--     { "logger":{"Stderr":{}}
--     , "filters":
--         { "SQL":"Error"
--         , "" : "Debug"
--         }
--     }
--     }
-- ,   { "logger": {"Journald":{"syslogIdentifier":"<myAppName>"}}
--     }
-- ]
--
-- groundhog/postgresql-simple also use monad-logger, so you could also add:
--
-- ,   { "logger":{"File":{"file":"sql.log"}}
--     , "filters":{"SQL":"Debug"}
--     }
module Rhyolite.Logging
  ( withLogging
  , withLoggingMinLevel
  , LoggingEnv(..)
  , LoggingConfig(..)
  , runLoggingEnv
  , filterLog
  , LogAppender
  , getLogContext
  , RhyoliteLogAppender(..)
  , RhyoliteLogLevel(..)
  , RhyoliteLogAppenderStderr (..)
  , RhyoliteLogAppenderFile (..)
#if defined(SUPPORT_SYSTEMD_JOURNAL)
  , RhyoliteLogAppenderJournald (..)
#endif
  ) where

import Control.Monad (when)
import Control.Monad.Catch (MonadMask, finally)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger
  ( Loc (..), LoggingT (..), LogLevel (..), LogSource
  , logDebug, logError, logInfo, logWarn
  )
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Lazy as LBS
import Data.Default (Default (def))
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid (Monoid, mappend, mempty)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Trie.BigEndianPatricia.Base as Trie
import GHC.Generics (Generic)
import System.Log.FastLogger (BufSize, FileLogSpec (..), LogStr, LogType (..), TimedFastLogger, toLogStr)
import qualified System.Log.FastLogger as FastLogger

#if defined(SUPPORT_SYSTEMD_JOURNAL)
import qualified Data.HashMap.Strict as HashMap
import qualified Systemd.Journal as Journal
#endif

newtype LoggingEnv =  LoggingEnv { unLoggingEnv :: Loc -> LogSource -> LogLevel -> LogStr -> IO () }

data RhyoliteLogLevel
  = RhyoliteLogLevel_Debug
  | RhyoliteLogLevel_Info
  | RhyoliteLogLevel_Warn
  | RhyoliteLogLevel_Error
  deriving (Eq, Ord, Show, Generic, Enum)

toLogLevel :: RhyoliteLogLevel -> LogLevel
toLogLevel RhyoliteLogLevel_Debug = LevelDebug
toLogLevel RhyoliteLogLevel_Info = LevelInfo
toLogLevel RhyoliteLogLevel_Warn = LevelWarn
toLogLevel RhyoliteLogLevel_Error = LevelError

data LoggingConfig a = LoggingConfig
  { _loggingConfig_logger :: a
  , _loggingConfig_filters :: Maybe (Map T.Text RhyoliteLogLevel)
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

instance Default a => Default (LoggingConfig a) where
  def = LoggingConfig def Nothing

instance Semigroup LoggingEnv where
  LoggingEnv x <> LoggingEnv y = LoggingEnv $ x `mappend` y

-- Bleh.  i can't say deriving newtype (Monoid) since i don't have DerivingStrateges yet.
instance Monoid LoggingEnv where
  mempty = LoggingEnv mempty
  mappend = (<>)

data LoggingContext m = LoggingContext
  { _loggingContext_cleanup :: m ()
  , _loggingContext_logger :: LoggingEnv
  }

data RhyoliteLogAppender
   = RhyoliteLogAppender_Stderr   RhyoliteLogAppenderStderr
   | RhyoliteLogAppender_File     RhyoliteLogAppenderFile
#if defined(SUPPORT_SYSTEMD_JOURNAL)
   | RhyoliteLogAppender_Journald RhyoliteLogAppenderJournald -- journalctl log with syslogIdentifier specified.
#endif
   | RhyoliteLogAppender_FileRotate RhyoliteLogAppenderFileRotate
   -- fastlogger also supports time based rotation, but its API calls for a function which must parse a pair of "human readable" ByteStrings as dates.  A buggy implementation of half of common lisp in json syntax is out of scope for this yakshave.
  deriving (Generic, Eq, Ord, Show)

instance Default RhyoliteLogAppender where
  def = RhyoliteLogAppender_Stderr def

data RhyoliteLogAppenderStderr = RhyoliteLogAppenderStderr
  { _rhyoliteLogAppenderStderr_placeholder :: Maybe ()  -- force json encoding to be a record; i'd use Maybe Void, but Void has no FromJSON, but `Maybe Void` would overlap.
  } deriving (Generic, Eq, Ord, Show)

instance Default RhyoliteLogAppenderStderr where
  def = RhyoliteLogAppenderStderr Nothing

data RhyoliteLogAppenderFile = RhyoliteLogAppenderFile
  { _rhyoliteLogAppenderFile_file :: !FilePath
  } deriving (Generic, Eq, Ord, Show)

data RhyoliteLogAppenderFileRotate = RhyoliteLogAppenderFileRotate
  { _rhyoliteLogAppenderFileRotate_file :: !FilePath
  , _rhyoliteLogAppenderFileRotate_filesize :: !Integer
  , _rhyoliteLogAppenderFileRotate_backups :: !Int
  } deriving (Generic, Eq, Ord, Show)

#if defined(SUPPORT_SYSTEMD_JOURNAL)
data RhyoliteLogAppenderJournald = RhyoliteLogAppenderJournald
  { _rhyoliteLogAppenderJournald_syslogIdentifier :: T.Text -- journalctl log with syslogIdentifier specified.
  } deriving (Generic, Eq, Ord, Show)
#endif

-- derive a little differently from `Rhyolite.Request.makeJson` so that more
-- things end up as records, so that new fields don't break old configs
fmap concat $ traverse (deriveJSON Aeson.defaultOptions
    { Aeson.sumEncoding = Aeson.ObjectWithSingleField
    , Aeson.constructorTagModifier = dropWhile ('_' ==) . dropWhile ('_' /=)
    , Aeson.fieldLabelModifier = dropWhile ('_' ==) . dropWhile ('_' /=) . dropWhile ('_' ==)
    , Aeson.omitNothingFields = True
    })
  [ ''RhyoliteLogLevel
  , ''RhyoliteLogAppender
  , ''RhyoliteLogAppenderStderr
  , ''RhyoliteLogAppenderFile
  , ''RhyoliteLogAppenderFileRotate
#if defined(SUPPORT_SYSTEMD_JOURNAL)
  , ''RhyoliteLogAppenderJournald
#endif
  , ''LoggingConfig
  ]

runLoggingEnv :: LoggingEnv -> LoggingT m a -> m a
runLoggingEnv = flip runLoggingT . unLoggingEnv

logToFastLogger :: TimedFastLogger -> LoggingEnv
logToFastLogger ls = LoggingEnv $ \_loc logSource logLevel logStr -> ls (\ft -> toLogStr ft <> toLogStr (show logLevel) <> toLogStr (show logSource) <> logStr <> "\n")

#if defined(SUPPORT_SYSTEMD_JOURNAL)
logger2journald :: LogLevel -> Journal.JournalFields
logger2journald = \case
  LevelWarn -> Journal.priority Journal.Warning
  LevelDebug -> Journal.priority Journal.Debug
  LevelInfo -> Journal.priority Journal.Info
  LevelError -> Journal.priority Journal.Error
  LevelOther level -> Journal.priority Journal.Error  -- Error because that makes this logger2journald monotone
    <> mkJournalField' "PRIORITY_OTHER" level
#endif

class LogAppender a where
  getLogContext :: MonadIO m => a -> m (LoggingContext m)


fastLoggerHelperThing :: MonadIO m => (BufSize -> LogType) -> m (LoggingContext m)
fastLoggerHelperThing typ = do
  -- logSet :: <- liftIO $ newStderrLoggerSet FastLogger.defaultBufSize
  timer <- liftIO $ FastLogger.newTimeCache FastLogger.simpleTimeFormat'
  (tfl, cleanup) <- liftIO $ FastLogger.newTimedFastLogger timer $ typ FastLogger.defaultBufSize
  return $ LoggingContext (liftIO cleanup) (logToFastLogger tfl)

instance LogAppender RhyoliteLogAppender where
  getLogContext = \case
    RhyoliteLogAppender_Stderr _ ->
      fastLoggerHelperThing LogStderr
    RhyoliteLogAppender_File (RhyoliteLogAppenderFile filename) ->
      fastLoggerHelperThing $ LogFileNoRotate filename
    RhyoliteLogAppender_FileRotate (RhyoliteLogAppenderFileRotate filename fileSize backupNumber) ->
      fastLoggerHelperThing $ LogFile (FileLogSpec filename fileSize backupNumber)
#if defined(SUPPORT_SYSTEMD_JOURNAL)
    RhyoliteLogAppender_Journald cfg -> return $ LoggingContext (return ()) (logToJournalCtl (Journal.syslogIdentifier $ _rhyoliteLogAppenderJournald_syslogIdentifier cfg))
#endif

configLogger :: (LogAppender a, MonadIO m) => Maybe LogLevel -> LoggingConfig a -> m (LoggingContext m)
configLogger minLoggedLvl (LoggingConfig ls fs) = do
  LoggingContext cleaner logger <- getLogContext ls
  return $ LoggingContext cleaner $ filterLog minLoggedLvl (fmap toLogLevel $ maybe M.empty id fs) logger

#if defined(SUPPORT_SYSTEMD_JOURNAL)
mkJournalField' :: T.Text -> T.Text -> Journal.JournalFields
mkJournalField' k v = HashMap.singleton (Journal.mkJournalField k) (TE.encodeUtf8 v)

loc2jf :: Loc -> Journal.JournalFields
loc2jf (Loc fn pkg _ _ (line, _)) = mconcat
  [ Journal.codeFile fn
  , Journal.codeLine line
  , mkJournalField' "code_package" (T.pack pkg)
  ]

logToJournalCtl :: Journal.JournalFields -> LoggingEnv
logToJournalCtl syslogId = LoggingEnv $ \loc logSource logLevel logStr ->
  Journal.sendMessageWith (TE.decodeUtf8 $ FastLogger.fromLogStr logStr) $ mconcat
    [ syslogId
    , logger2journald logLevel
    , loc2jf loc
    , mkJournalField' "logsource" logSource
    ]
#endif

-- | Match the LogSource on prefixes in m. If found, log only the messages at equal or higher than the
--   matched level. If not found and minLoggedLvl is Just value, then log minLoggedLvl or higher.
--   You can override the default with a zero length prefix, ie `"" := LevelDebug`
filterLog :: Maybe LogLevel -> Map T.Text LogLevel -> LoggingEnv -> LoggingEnv
filterLog minLoggedLvl m (LoggingEnv f) =
  let
    -- this gets checked a LOT.  so this needs to be hoisted out of the logging function.
    m' :: Trie.Trie LogLevel
    m' = Trie.fromList $ M.toList $ M.mapKeys TE.encodeUtf8 m
  in LoggingEnv $ \loc src lvl msg -> do
    let filterLvl = case Trie.match m' (TE.encodeUtf8 src) of
          Nothing -> minLoggedLvl
          Just (_, lvl', _) -> Just lvl'
    when (maybe False (lvl >=) filterLvl) $ do
      f loc src lvl msg

withLogging
  :: forall l m a. (LogAppender l, MonadMask m, MonadIO m)
  => [LoggingConfig l]
  -> LoggingT m a
  -> m a
withLogging = withLoggingMinLevel (Just LevelWarn)

withLoggingMinLevel
  :: forall l m a. (LogAppender l, MonadMask m, MonadIO m)
  => Maybe LogLevel -- ^ Min level to log in case of no filter match. Specify (Just LevelWarn) to log errors and warnings even if they dont match any filter.
  -> [LoggingConfig l]
  -> LoggingT m a
  -> m a
withLoggingMinLevel minLoggedLvl configs (LoggingT x) = do
  cleanersAndLoggers <- traverse (configLogger minLoggedLvl) configs
  let cleaner = foldl' (>>) (pure ())  $ fmap _loggingContext_cleanup cleanersAndLoggers
  let (LoggingEnv logger) = foldMap _loggingContext_logger cleanersAndLoggers
  finally (x logger) cleaner


-- Compiled documentation...
_rhyoliteLoggingExample_ :: FilePath -> IO ()
_rhyoliteLoggingExample_ f = do
  putStrLn $ T.unpack $ TE.decodeUtf8 $ LBS.toStrict $ Aeson.encode
    [ LoggingConfig (RhyoliteLogAppender_Stderr $ RhyoliteLogAppenderStderr Nothing) Nothing
    , LoggingConfig (RhyoliteLogAppender_File $ RhyoliteLogAppenderFile "/dev/null") Nothing
    , LoggingConfig (RhyoliteLogAppender_Stderr $ RhyoliteLogAppenderStderr Nothing) (Just $ M.fromList [("context",RhyoliteLogLevel_Debug)])
#if defined(SUPPORT_SYSTEMD_JOURNAL)
    , LoggingConfig (RhyoliteLogAppender_Journald $ RhyoliteLogAppenderJournald "foo") Nothing
#endif
    ]
  withLogging @ RhyoliteLogAppender [def] $ do
    $(logError) "Err"
    $(logWarn) "Warn"
    $(logInfo) "Info"
    $(logDebug) "Debug"

  confs :: [LoggingConfig RhyoliteLogAppender] <- either (error . (("JSON decode error while reading file " <> f <> ":") <>)) id . Aeson.eitherDecode' <$> LBS.readFile f

  withLogging confs $ do
    $(logDebug) "Hello World"
