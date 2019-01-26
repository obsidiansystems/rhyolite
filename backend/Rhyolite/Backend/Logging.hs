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

-- | typically useful config file, which logs everything to stderr and warnings to journald.
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
  , RhyoliteLogAppenderStderr (..)
  , RhyoliteLogAppenderFile (..)
#ifdef linux_HOST_OS
  , RhyoliteLogAppenderJournald (..)
#endif
  , example
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson hiding (Error)
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HashMap
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid (Monoid, mappend, mempty)
import Data.Semigroup (Semigroup, (<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Trie.BigEndianPatricia.Base as Trie
import GHC.Generics
import System.Log.FastLogger

#ifdef linux_HOST_OS
import Systemd.Journal
#endif

import Data.Default

import Data.Semigroup

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
#ifdef linux_HOST_OS
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

#ifdef linux_HOST_OS
data RhyoliteLogAppenderJournald = RhyoliteLogAppenderJournald
  { _rhyoliteLogAppenderJournald_syslogIdentifier :: T.Text -- journalctl log with syslogIdentifier specified.
  } deriving (Generic, Eq, Ord, Show)
#endif

-- derive a little differently from `Rhyolite.Request.makeJson` so that more
-- things end up as records, so that new fields don't break old configs
fmap concat $ traverse (deriveJSON defaultOptions
    { sumEncoding = ObjectWithSingleField
    , constructorTagModifier = dropWhile ('_' ==) . dropWhile ('_' /=)
    , fieldLabelModifier = dropWhile ('_' ==) . dropWhile ('_' /=) . dropWhile ('_' ==)
    , omitNothingFields = True
    })
  [ ''RhyoliteLogLevel
  , ''RhyoliteLogAppender
  , ''RhyoliteLogAppenderStderr
  , ''RhyoliteLogAppenderFile
  , ''RhyoliteLogAppenderFileRotate
#ifdef linux_HOST_OS
  , ''RhyoliteLogAppenderJournald
#endif
  , ''LoggingConfig
  ]

runLoggingEnv :: LoggingEnv -> LoggingT m a -> m a
runLoggingEnv = flip runLoggingT . unLoggingEnv

logToFastLogger :: TimedFastLogger -> LoggingEnv
logToFastLogger ls = LoggingEnv $ \_loc logSource logLevel logStr -> ls (\ft -> toLogStr ft <> toLogStr (show logLevel) <> toLogStr (show logSource) <> logStr <> "\n")

#ifdef linux_HOST_OS
logger2journald :: LogLevel -> JournalFields
logger2journald = \case
  LevelWarn -> priority Warning
  LevelDebug -> priority Debug
  LevelInfo -> priority Info
  LevelError -> priority Error
  LevelOther level -> priority Error  -- Error because that makes this logger2journald monotone
    <> mkJournalField' "PRIORITY_OTHER" level
#endif

class LogAppender a where
  getLogContext :: MonadIO m => a -> m (LoggingContext m)



fastLoggerHelperThing :: MonadIO m => (BufSize -> LogType) -> m (LoggingContext m)
fastLoggerHelperThing typ = do
  -- logSet :: <- liftIO $ newStderrLoggerSet defaultBufSize
  timer <- liftIO $ newTimeCache simpleTimeFormat'
  (tfl, cleanup) <- liftIO $ newTimedFastLogger timer $ typ defaultBufSize
  return $ LoggingContext (liftIO cleanup) (logToFastLogger tfl)

instance LogAppender RhyoliteLogAppender where
  getLogContext = \case
      RhyoliteLogAppender_Stderr _ ->
        fastLoggerHelperThing LogStderr
      RhyoliteLogAppender_File (RhyoliteLogAppenderFile filename) ->
        fastLoggerHelperThing $ LogFileNoRotate filename
      RhyoliteLogAppender_FileRotate (RhyoliteLogAppenderFileRotate filename fileSize backupNumber) ->
        fastLoggerHelperThing $ LogFile (FileLogSpec filename fileSize backupNumber)
#ifdef linux_HOST_OS
      RhyoliteLogAppender_Journald cfg -> return $ LoggingContext (return ()) (logToJournalCtl (syslogIdentifier $ _rhyoliteLogAppenderJournald_syslogIdentifier cfg))
#endif

configLogger :: (LogAppender a, MonadIO m) => LoggingConfig a -> m (LoggingContext m)
configLogger (LoggingConfig ls fs) = do
  LoggingContext cleaner logger <- getLogContext ls
  return $ LoggingContext cleaner $ filterLog (fmap toLogLevel $ maybe M.empty id fs) logger

#ifdef linux_HOST_OS
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
#endif

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
withLogging :: forall l m a. (LogAppender l, MonadMask m, MonadIO m) => [LoggingConfig l] -> LoggingT m a -> m a
withLogging configs (LoggingT x) = do
  cleanersAndLoggers <- traverse configLogger configs
  let cleaner = foldl' (>>) (pure ())  $ fmap _loggingContext_cleanup cleanersAndLoggers
  let (LoggingEnv logger) = foldMap _loggingContext_logger cleanersAndLoggers
  finally (x logger) cleaner

example :: FilePath -> IO ()
example f = do
  putStrLn $ T.unpack $ TE.decodeUtf8 $ LBS.toStrict $ encode
    [ LoggingConfig (RhyoliteLogAppender_Stderr $ RhyoliteLogAppenderStderr Nothing) Nothing
    , LoggingConfig (RhyoliteLogAppender_File $ RhyoliteLogAppenderFile "/dev/null") Nothing
    , LoggingConfig (RhyoliteLogAppender_Stderr $ RhyoliteLogAppenderStderr Nothing) (Just $ M.fromList [("context",RhyoliteLogLevel_Debug)])
#ifdef linux_HOST_OS
    , LoggingConfig (RhyoliteLogAppender_Journald $ RhyoliteLogAppenderJournald "foo") Nothing
#endif
    ]
  withLogging @ RhyoliteLogAppender [def] $ do
    $(logError) "Err"
    $(logWarn) "Warn"
    $(logInfo) "Info"
    $(logDebug) "Debug"

  confs :: [LoggingConfig RhyoliteLogAppender] <- either (error . (("JSON decode error while reading file " <> f <> ":") <>)) id . eitherDecode' <$> LBS.readFile f

  withLogging confs $ do
    $(logDebug) "Hello World"
