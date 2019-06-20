{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- Module extracted from focus. Got rid of all the stuff that is not
-- needed and changed parts to use Rhyolite instead of Focus stuff (e.g.
-- logging):
module Rhyolite.Backend.HaveIBeenPwned where

import "cryptohash" Crypto.Hash -- or maybe i wanted cryptonite?
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text(Text)
import Data.Text.Encoding(encodeUtf8)
import qualified Data.Text.Lazy.Encoding
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status(Status(..))
import System.IO
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Default (def)
import Safe (readMay)

import Rhyolite.Backend.Logging (withLogging, LoggingConfig (..), RhyoliteLogAppender (..))


data HaveIBeenPwnedError
  = HaveIBeenPwnedError_BadResponse Status
  | HaveIBeenPwnedError_NetworkError HttpException
  deriving (Show) -- no eq instance for HttpException

data HaveIBeenPwnedConfig = HaveIBeenPwnedConfig
  { _haveIBeenPwnedConfig_manager :: Manager
  , _haveIBeenPwnedConfig_apihost :: Text
  }

data HaveIBeenPwnedResult
  = HaveIBeenPwnedResult_Undisclosed
  | HaveIBeenPwnedResult_Disclosed Int
  | HaveIBeenPwnedResult_ApiError
  deriving (Eq, Ord, Show)

class Monad m => MonadPwned m where
  -- | Returns the number of disclosures the supplied password has been seen in.
  --
  -- If this is not zero, do not use the supplied password, it is known to hackers.
  -- If it *is* zero, it might still not be safe, only that if it is
  -- compromised, that is not yet known.
  --
  -- https://haveibeenpwned.com/API/v2#SearchingPwnedPasswordsByRange
  haveIBeenPwned :: Text -> m HaveIBeenPwnedResult

newtype PwnedT m a = PwnedT { unPwnedT :: ReaderT HaveIBeenPwnedConfig m a }
  deriving (Functor, Applicative, Monad , MonadIO, MonadLogger
    , MonadTrans
    )

runPwnedT :: PwnedT m a -> HaveIBeenPwnedConfig -> m a
runPwnedT (PwnedT (ReaderT f)) = f

mapPwnedT :: (m a -> n b) -> PwnedT m a -> PwnedT n b
mapPwnedT f = PwnedT . mapReaderT f . unPwnedT

instance MonadReader r m => MonadReader r (PwnedT m) where
  ask = lift ask
  local = mapPwnedT . local
  reader = lift . reader

instance (MonadLogger m, MonadIO m) => MonadPwned (PwnedT m) where
 haveIBeenPwned password = do
  let (pfx, rest) = passwdDigest password
  cfg <- PwnedT ask
  let request = parseRequest_ $ T.unpack $ T.concat [_haveIBeenPwnedConfig_apihost cfg, "/", pfx]
  result' <- liftIO $ try $ httpLbs request (_haveIBeenPwnedConfig_manager cfg)
  case result' of
    Left err -> do
      $(logError) $ T.pack $ show @ HttpException $ err
      return HaveIBeenPwnedResult_ApiError
    Right result -> case responseStatus result of
      Status 200 _ -> case parseHIBPResponse (responseBody result) rest of
        Just 0 -> pure HaveIBeenPwnedResult_Undisclosed
        Just n -> pure $ HaveIBeenPwnedResult_Disclosed n
        Nothing -> do
          $(logError) $ "Parsing number of occurrences failed. (Not an Int)."
          pure HaveIBeenPwnedResult_ApiError
      Status code phrase -> do
        $(logError) $ T.pack $ show $ Status code phrase
        return HaveIBeenPwnedResult_ApiError


-- | Get the sha1 digest for the supplied password, split into two parts, to agree with the
--   hibp api.
passwdDigest :: Text -> (Text, Text)
passwdDigest passwd = (T.take 5 digest, T.drop 5 digest)
  where digest = T.toUpper $ T.pack $ show $ sha1 $ encodeUtf8 passwd
        sha1 :: ByteString -> Digest SHA1
        sha1 = hash

-- | The hibp response is a line separated list of colon separated hash
-- *suffixes* and a number indicating the number of times that password(hash)
-- has been seen in known publicly disclosed leaks
parseHIBPResponse :: LBS.ByteString -> Text -> Maybe Int
parseHIBPResponse response suffix =
  let
    digests :: [(LT.Text, Maybe Int)]
    digests = fmap (fmap (readMay . LT.unpack . LT.drop 1) . LT.breakOn ":") $ LT.lines $ Data.Text.Lazy.Encoding.decodeUtf8 response
  in case filter ((LT.fromStrict suffix ==) . fst) digests of
    ((_,n):_) -> n
    [] -> Just 0

-- a really simple demo of the hibp functionality
consoleHaveIBeenPwned :: IO ()
consoleHaveIBeenPwned = do
  withLogging [LoggingConfig (RhyoliteLogAppender_Stderr def) mempty] $ do
    mgr <- liftIO $ newManager tlsManagerSettings
    p <- liftIO $ getPassword
    let hibpEnv = HaveIBeenPwnedConfig mgr "https://api.pwnedpasswords.com/range"
    p' <- flip runPwnedT hibpEnv $ haveIBeenPwned $ T.pack p
    liftIO $ case p' of
      HaveIBeenPwnedResult_Disclosed p'' ->
        putStrLn $ "You have been pwned!  your password has appeared in breaches " ++ show p'' ++ " times."
      HaveIBeenPwnedResult_Undisclosed ->
        putStrLn "Your password does not appear in any known breaches.  Practice good password hygene."
      HaveIBeenPwnedResult_ApiError ->
        putStrLn "Network Error, try again later"

getPassword :: IO String
getPassword = do
  putStr "Password: "
  hFlush stdout
  password <- withEcho False getLine
  putChar '\n'
  return password

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
