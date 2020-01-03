-- | The implementation for a mail queue and relative worker in the backend.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rhyolite.Backend.EmailWorker where

import Control.Exception.Lifted (bracket)
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.Trans.Control
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import Data.Pool
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.Groundhog.Postgresql hiding (Cond)
import Database.Groundhog.TH
import Database.Id.Class
import Database.Id.Groundhog
import Database.Id.Groundhog.TH
import qualified Network.Mail.Mime as Mail
import qualified Network.HaskellNet.SMTP as SMTP

import Rhyolite.Concurrent
import qualified Data.Map.Monoidal as Map
import Rhyolite.Backend.DB
import Rhyolite.Backend.DB.LargeObjects
import Rhyolite.Backend.DB.PsqlSimple
import Rhyolite.Backend.DB.Serializable (Serializable)
import qualified Rhyolite.Backend.DB.Serializable as Serializable
import Rhyolite.Backend.Email
import Rhyolite.Backend.Schema.TH
import Rhyolite.Schema

-- | Emails waiting to be sent
data QueuedEmail = QueuedEmail
  { _queuedEmail_mail :: LargeObjectId
  , _queuedEmail_to :: Json [Mail.Address]
  , _queuedEmail_from :: Json Mail.Address
  , _queuedEmail_expiry :: Maybe UTCTime
  , _queuedEmail_checkedOut :: Bool
  }

instance HasId QueuedEmail

mailToQueuedEmail :: (Monad m, PostgresLargeObject m, MonadBase Serializable m) => Mail.Mail -> Maybe UTCTime -> m QueuedEmail
mailToQueuedEmail m t = do
  -- We can safely lift 'renderMail' into serializable transactions because
  -- it's 'IO' is to generate a random boundary. If we retry we'll generate
  -- a different one but we don't care what it is anyway.
  r <- liftBase $ Serializable.unsafeMkSerializable $ liftIO $ Mail.renderMail' m
  (oid, _) <- newLargeObjectLBS r
  return $ QueuedEmail { _queuedEmail_mail = oid
                       , _queuedEmail_to = Json $ Mail.mailTo m
                       , _queuedEmail_from = Json $ Mail.mailFrom m
                       , _queuedEmail_expiry = t
                       , _queuedEmail_checkedOut = False
                       }

mkRhyolitePersist (Just "migrateQueuedEmail") [groundhog|
  - entity: QueuedEmail
    constructors:
      - name: QueuedEmail
        fields:
          - name: _queuedEmail_checkedOut
            default: "false"
|]

makeDefaultKeyIdInt64 ''QueuedEmail 'QueuedEmailKey

-- | Queues a single email
queueEmail :: (PostgresLargeObject m, PersistBackend m, MonadBase Serializable m)
          => Mail.Mail
          -> Maybe UTCTime
          -> m (Id QueuedEmail)
queueEmail m t = do
  qm <- mailToQueuedEmail m t
  fmap toId . insert $ qm

-- | Retrieves and sends emails one at a time, deleting them from the queue.
--   Guarantees "at most once" semantics, e.g. a power fault in the middle of
--   this process may leave some emails unsent, but no emails will be sent
--   twice.
clearMailQueue :: forall m f.
  ( RunDb f
  , MonadIO m
  , MonadLoggerIO m
  )
  => f (Pool Postgresql)
  -> EmailEnv
  -> m ()
clearMailQueue db emailEnv = do
  -- First we obtain a "lock" for an email we plan to send.
  queuedEmail <- runDb db $ do
    qe <- listToMaybe . Map.toList <$>
      selectMap' QueuedEmailConstructor ((QueuedEmail_checkedOutField ==. False) `limitTo` 1)
    forM_ (fst <$> qe) $ \eid ->
      update [QueuedEmail_checkedOutField =. True] $ AutoKeyField ==. fromId eid
    return qe
  case queuedEmail of
    Nothing -> return ()
    Just (qid, QueuedEmail oid (Json to) (Json from) expiry _) -> do
      -- With the lock held we can safely run a read-only transaction at lower isolation without retries.
      runDbReadOnlyRepeatableRead db $ do
        now <- getTime
        let
          notExpired = case expiry of
            Nothing -> True
            Just expiry' -> now < expiry'
        when notExpired $ do
          withStreamedLargeObject oid $ \payload ->
            sendQueuedEmail emailEnv from to (LBS.toStrict payload)
          $logInfo $ T.pack $ mconcat
            [ "["
            , show now
            , "] "
            , "Sending email to: "
            , show to
            ]

      -- "Release" the lock by removing this email from the queue.
      -- If an exception beats us to this point then we're still consistent with
      -- "at most once" semantics.
      runDb db $ do
        _ <- execute [sql| DELETE FROM "QueuedEmail" q WHERE q.id = ? |] (Only qid)
        deleteLargeObject oid
      clearMailQueue db emailEnv

sendQueuedEmail :: EmailEnv -> Mail.Address -> [Mail.Address] -> ByteString -> IO ()
sendQueuedEmail env sender recipients payload = do
  let from = T.unpack . Mail.addressEmail $ sender
      to = map (T.unpack . Mail.addressEmail) recipients
  void $ withSMTP env $ SMTP.sendMail from to payload

-- | Spawns a thread to monitor mail queue table and send emails if necessary
emailWorker :: (MonadLoggerIO m, RunDb f)
            => Int -- ^ Thread delay
            -> f (Pool Postgresql)
            -> EmailEnv
            -> m (IO ()) -- ^ Action that kills the email worker thread
emailWorker delay db emailEnv = askLoggerIO >>= worker delay . runLoggingT (clearMailQueue db emailEnv)

-- | Bracketed version of 'emailWorker'
withEmailWorker
  :: (MonadIO m, MonadBaseControl IO m, MonadLoggerIO m, RunDb f)
  => Int -- ^ Thread delay
  -> f (Pool Postgresql)
  -> EmailEnv
  -> m a
  -> m a
withEmailWorker i p e = bracket (emailWorker i p e) liftIO . const


deriveJSON defaultOptions ''Mail.Address
deriveJSON defaultOptions ''Mail.Encoding

-- These require orphan instances for ByteString
--deriveJSON defaultOptions ''Mail.Part
--deriveJSON defaultOptions ''Mail.Mail
