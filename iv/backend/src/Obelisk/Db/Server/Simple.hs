{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Obelisk.Db.Server.Simple where

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

withSimpleDbServer
  :: forall db request view
  .  _
  => AnnotatedDatabaseSettings Postgres db
  -> (forall a. request a -> Pg a)
  -> LiveQuery db view
  -> (Snap () -> IO ())
  -> IO ()
withSimpleDbServer checkedDbSchema handleRequest view k = withDbUri "db" $ \dbUri -> withConnectionPool dbUri $ \dbConnPool -> do
  let dbSchema = deAnnotateDatabase checkedDbSchema
      myLog = \_ -> pure () -- T.putStrLn
      requestHandler :: RequestHandler request IO
      requestHandler = RequestHandler $ \req -> withResource dbConnPool $ \dbConn -> runBeamPostgresDebug myLog dbConn $ handleRequest req
  withResource dbConnPool $ tryRunMigrationsWithEditUpdateAndHooks (pure ()) (\_ -> pure ()) id (fromAnnotatedDbSettings checkedDbSchema $ Proxy @'[])
  serveDbOverWebsocketsNew @db @request
    myLog
    dbUri
    dbSchema
    requestHandler
    (\(QueryResultPatch d) (IView q) -> fmap (IView . mapV (ResultV . runIdentity)) $ _liveQuery_listen view dbSchema d $ mapV (\_ -> Proxy) q)
    (\(IView q) -> fmap (IView . mapV (ResultV . runIdentity)) $ _liveQuery_view view dbSchema $ mapV (\_ -> Proxy) q)
    (viewPipeline (\(Const ()) -> QueryV) (\(ResultV x) -> Identity x))
    $ \_serviceRegistrar serveApi -> k serveApi
