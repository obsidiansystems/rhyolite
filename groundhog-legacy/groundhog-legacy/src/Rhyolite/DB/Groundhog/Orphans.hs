{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language StandaloneDeriving #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}
{-# Language UndecidableInstances #-}
{-# options_ghc -fno-warn-orphans #-}
module Rhyolite.DB.Groundhog.Orphans where

import Control.Exception.Lifted (bracket)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Control
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Signed
import Data.Signed.ClientSession as CS
import Data.Text (Text)
import Database.Groundhog.Postgresql
import Database.Groundhog.Postgresql.Array
import Database.Id.Class
import Database.PostgreSQL.Simple.Class
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Groundhog
import qualified Database.PostgreSQL.Simple.LargeObjects as Sql
import qualified Database.PostgreSQL.Simple.LargeObjects.Stream as LO
import Database.PostgreSQL.Simple.ToField
import Rhyolite.DB.Groundhog.Serializable
import Rhyolite.DB.Groundhog.TH
import qualified System.IO.Streams as Streams

instance (FromField (IdData a)) => FromField (Id a) where
  fromField f mbs = fmap Id (fromField f mbs)

instance (ToField (IdData a)) => ToField (Id a) where
  toField (Id x) = toField x

deriving instance Ord (Array BS.ByteString)
deriving instance Ord (Array LBS.ByteString)
deriving instance Ord (Array Text)

deriving instance Read (Array BS.ByteString)
deriving instance Read (Array LBS.ByteString)
deriving instance Read (Array Text)

instance (MonadIO m, MonadBaseControl IO m) => PostgresLargeObject (DbPersist Postgresql m) where
  newEmptyLargeObject = fmap fromOid $ liftWithConn $ \conn -> Sql.loCreat conn
  withLargeObject oid mode =
    bracket (liftWithConn $ \conn -> Sql.loOpen conn (toOid oid) mode)
            (\lofd -> liftWithConn $ \conn -> Sql.loClose conn lofd)
  newLargeObjectFromFile filePath =
    liftWithConn $ \conn -> fmap fromOid $ Sql.loImport conn filePath
  newLargeObjectBS contents =
    fmap fromOid $ liftWithConn $ \conn -> LO.newLargeObjectBS conn contents
  newLargeObjectLBS =
    newLargeObjectStream <=< liftIO . Streams.fromLazyByteString
  newLargeObjectStream s = fmap (\(oid,sz) -> (fromOid oid, sz)) $
    liftWithConn $ \conn -> LO.newLargeObjectStream conn s
  streamLargeObject oid os =
    liftWithConn $ \conn -> LO.streamLargeObject conn (toOid oid) os
  streamLargeObjectRange oid start end os =
    liftWithConn (\conn -> LO.streamLargeObjectRange conn (toOid oid) start end os)
  deleteLargeObject oid = liftWithConn $ \conn -> Sql.loUnlink conn $ toOid oid

instance PostgresLargeObject Serializable where
  newEmptyLargeObject = unsafeLiftDbPersist newEmptyLargeObject
  withLargeObject oid mode f = unsafeLiftDbPersist $ withLargeObject oid mode (toDbPersist . f)
  newLargeObjectFromFile = unsafeLiftDbPersist . newLargeObjectFromFile
  newLargeObjectBS = unsafeLiftDbPersist . newLargeObjectBS
  newLargeObjectLBS = unsafeLiftDbPersist . newLargeObjectLBS
  newLargeObjectStream = unsafeLiftDbPersist . newLargeObjectStream
  streamLargeObject oid os = unsafeLiftDbPersist $ streamLargeObject oid os
  streamLargeObjectRange oid start end os = unsafeLiftDbPersist $ streamLargeObjectRange oid start end os
  deleteLargeObject = unsafeLiftDbPersist . deleteLargeObject

deriveNewtypePersistBackend (\m -> [t| SignT $m |]) (\m -> [t| ReaderT CS.Key $m |]) 'SignT 'unSignT

instance (Monad m, Psql m) => Psql (SignT m)
instance (Monad m, PostgresLargeObject m) => PostgresLargeObject (SignT m) where
  withLargeObject oid mode f = do
    k <- SignT ask
    lift $ withLargeObject oid mode (\lofd -> runSignT (f lofd) k)

instance MonadSign m => MonadSign (DbPersist conn m) where
  type SigningKey (DbPersist conn m) = SigningKey m
