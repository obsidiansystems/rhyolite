{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language MultiParamTypeClasses #-}
{-# Language QuantifiedConstraints #-}
{-# Language ScopedTypeVariables #-}
{-# Language RankNTypes #-}
module Database.PostgreSQL.Serializable
  ( Serializable
  , unsafeMkSerializable
  , unSerializable
  , runSerializable
  , runSerializableInsideTransaction
  , unsafeHoistCoerceFromReaderT
  ) where

import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Logger.Extras
import Control.Monad.Trans.Reader
import Data.Coerce
import Data.Pool
import qualified Database.PostgreSQL.Simple as Pg
import Database.PostgreSQL.Simple.Class
import qualified Database.PostgreSQL.Simple.Transaction as Pg

-- | A monad for database transactions with serializable isolation level.
--
-- Because this monad may retry execution of code automatically, it does not lawfully lift any effects other
-- than Reader, which it commutes with, so there is no gain in having it be a transformer.
--
-- It "disallows" (makes harder) arbitrary IO.
-- It "disallows" (makes harder) catching IO exceptions *inside* the transaction.
newtype Serializable a = Serializable (ReaderT Pg.Connection (LoggingT IO) a)
  deriving (Functor, Applicative, Monad, MonadThrow, MonadLogger)
  -- NOTE: We *intentionally* leave out
  --   - 'MonadCatch' so you can't accidentally mask a serialization error from the outer retry logic.
  --   - 'MonadBaseControl' (et al) for the same reason.
  --   - 'MonadIO' so you can't execute arbitrary IO.

instance MonadBase Serializable Serializable where
  liftBase = id

unsafeMkSerializable :: ReaderT Pg.Connection (LoggingT IO) a -> Serializable a
unsafeMkSerializable = Serializable

unSerializable :: Serializable a -> ReaderT Pg.Connection (LoggingT IO) a
unSerializable (Serializable m) = m

runSerializable :: forall a m. (MonadIO m) => Pool Pg.Connection -> Logger -> Serializable a -> m a
runSerializable pool logger (Serializable act) = liftIO $ withResource pool $ \c ->
  Pg.withTransactionSerializable c $
    runLoggerLoggingT (runReaderT act c) logger

runSerializableInsideTransaction :: forall a m. (MonadIO m) => Pg.Connection -> Logger -> Serializable a -> m a
runSerializableInsideTransaction conn logger (Serializable act) = liftIO $ runLoggerLoggingT (runReaderT act conn) logger

unsafeHoistCoerceFromReaderT :: forall f. (forall a b. Coercible a b => Coercible (f a) (f b)) => f (ReaderT Pg.Connection (LoggingT IO)) -> f Serializable 
unsafeHoistCoerceFromReaderT = coerce

instance Psql Serializable where
  askConn = Serializable ask
  execute p q = Serializable $ do
    conn <- ask
    liftIO $ Pg.execute conn p q
  execute_ q = Serializable $ ask >>= \conn -> liftIO $ Pg.execute_ conn q
  executeMany p q = Serializable $ ask >>= \conn -> liftIO $ Pg.executeMany conn p q
  query p q = Serializable $ ask >>= \conn -> liftIO $ Pg.query conn p q
  query_ q = Serializable $ ask >>= \conn -> liftIO $ Pg.query_ conn q
  queryWith parser p q = Serializable $ ask >>= \conn -> liftIO $ Pg.queryWith parser conn p q
  queryWith_ parser q = Serializable $ ask >>= \conn -> liftIO $ Pg.queryWith_ parser conn q
  formatQuery p q = Serializable $ ask >>= \conn -> liftIO $ Pg.formatQuery conn p q
  returning p q = Serializable $ ask >>= \conn -> liftIO $ Pg.returning conn p q
