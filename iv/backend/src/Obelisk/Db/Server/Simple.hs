{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Obelisk.Db.Server.Simple where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Default
import Data.Functor.Const
import Data.Functor.Identity
import Data.Pool
import Data.Vessel
import Database.Beam.AutoMigrate
import Database.Beam.Postgres
import Obelisk.Beam.Patch.Db
import Obelisk.Db
import Obelisk.Db.LiveQuery
import Obelisk.View.App
import Obelisk.View.Vessel
import Rhyolite.Backend.App
import Snap.Core

data SimpleDbServerConfig db = SimpleDbServerConfig
  { _simpleDbServerConfig_schema :: AnnotatedDatabaseSettings Postgres db
  , _simpleDbServerConfig_options :: SimpleDbServerOptions db
  }

data SimpleDbServerOptions db = forall a. SimpleDbServerOptions
  { _simpleDbServerOptions_dbPath :: Text
  , _simpleDbServerOptions_preMigration :: Pg a
  , _simpleDbServerOptions_postMigration :: a -> Pg ()
  , _simpleDbServerOptions_editMigrationUpdates :: [WithPriority Edit] -> [WithPriority Edit]
  , _simpleDbServerOptions_logger :: Text -> IO ()
  }

instance Default (SimpleDbServerOptions db) where
  def = SimpleDbServerOptions
    { _simpleDbServerOptions_dbPath = "db"
    , _simpleDbServerOptions_preMigration = pure ()
    , _simpleDbServerOptions_postMigration = \_ -> pure ()
    , _simpleDbServerOptions_editMigrationUpdates = id
    , _simpleDbServerOptions_logger = \_ -> pure ()
    }

withSimpleDbServer
  :: forall db request view
  .  _
  => SimpleDbServerConfig db
  -> (forall a. request a -> Pg a)
  -> LiveQuery db view
  -> ((forall a. Pg a -> IO a) -> Snap () -> IO ())
  -> IO ()
withSimpleDbServer cfg handleRequest view k = do
  let opts = _simpleDbServerConfig_options cfg
  withDbUri (T.unpack $ _simpleDbServerOptions_dbPath opts) $ \dbUri -> withConnectionPool dbUri $ \dbConnPool -> do
    let checkedDbSchema = _simpleDbServerConfig_schema cfg
        dbSchema = deAnnotateDatabase checkedDbSchema
        myLog = _simpleDbServerOptions_logger opts
        runDb :: forall a. Pg a -> IO a
        runDb txn = withResource dbConnPool $ \dbConn -> runBeamPostgresDebug (myLog . T.pack) dbConn txn
        requestHandler :: RequestHandler request IO
        requestHandler = RequestHandler $ \req -> runDb $ handleRequest req
    withResource dbConnPool $ migrateSimpleDb cfg
    serveDbOverWebsocketsNew @db @request
      myLog
      dbUri
      dbSchema
      requestHandler
      (\(QueryResultPatch d) (IView q) -> fmap (IView . mapV (ResultV . runIdentity)) $ _liveQuery_listen view dbSchema d $ mapV (\_ -> Proxy) q)
      (\(IView q) -> fmap (IView . mapV (ResultV . runIdentity)) $ _liveQuery_view view dbSchema $ mapV (\_ -> Proxy) q)
      (viewPipeline (\(Const ()) -> QueryV) (\(ResultV x) -> Identity x))
      $ \_serviceRegistrar serveApi -> k runDb serveApi

migrateSimpleDb
  :: _
  => SimpleDbServerConfig db
  -> _
migrateSimpleDb cfg = do
  opts@(SimpleDbServerOptions
    { _simpleDbServerOptions_preMigration = preMigration
    , _simpleDbServerOptions_postMigration = postMigration
    })
    <- pure $ _simpleDbServerConfig_options cfg
  tryRunMigrationsWithEditUpdateAndHooks
    preMigration
    postMigration
    (_simpleDbServerOptions_editMigrationUpdates opts)
    (fromAnnotatedDbSettings (_simpleDbServerConfig_schema cfg) $ Proxy @'[])

runSimpleDbTransaction
  :: forall db a
  .  _
  => SimpleDbServerConfig db
  -> Pg a
  -> IO a
runSimpleDbTransaction cfg k = do
  let opts = _simpleDbServerConfig_options cfg
  withDb (T.unpack $ _simpleDbServerOptions_dbPath opts) $ \dbConnPool -> do
    withResource dbConnPool $ \dbConn -> do
      migrateSimpleDb cfg dbConn
      runBeamPostgresDebug (\_ -> pure ()) dbConn k
