{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Obelisk.Db.Server.Simple where

import Data.ByteString
import Data.Text (Text)
import qualified Data.Text as T
import Data.Default
import Data.Functor.Const
import Data.Functor.Identity
import Data.Map.Monoidal
import Data.Pool
import Data.Text.Encoding
import Data.Vessel
import Data.Vessel.SubVessel
import Database.Beam.AutoMigrate
import Database.Beam.Postgres
import Network.URI
import Obelisk.Beam.Patch.Db
import qualified Database.PostgreSQL.Simple as PG
import Obelisk.Api
import Obelisk.Db
import Obelisk.Db.LiveQuery
import Obelisk.View.App
import Obelisk.View.Vessel
import Rhyolite.Backend.App
import Snap.Core

data SimpleDbServerConfig db = SimpleDbServerConfig
  { _simpleDbServerConfig_schema :: AnnotatedDatabaseSettings Postgres db
  , _simpleDbServerConfig_migrationSchema :: AnnotatedDatabaseSettings Postgres db -> Schema
    -- ^ Ideally we would just have this be a 'Schema', that doesn't
    -- losslessly contain 'AnnotatedDatabaseSettings Postgres db'. (That
    -- 'Schema' doesn't need a 'db' type argument is a sign of this.)
    --
    -- We could store a 'Schema' and a 'AnnotatedDatabaseSettings
    -- Postgres db', but that is a bit denormalized as nothing ensures
    -- they are in sync.
    --
    -- Storing a function instead normalizes it. This cause be bad in
    -- that we loose sharing, but it turns out that is a non-issue as we
    -- only need the 'Schema' in one spot.
  , _simpleDbServerConfig_options :: SimpleDbServerOptions db
  }

-- | Default choice for '_simpleDbServerConfig_migrationSchema'
defaultMigrationSchema :: _ => AnnotatedDatabaseSettings Postgres db -> Schema
defaultMigrationSchema checkedDb = fromAnnotatedDbSettings checkedDb $ Proxy @'[]

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
  -> (forall a. request a -> WriteDb a)
  -> LiveQuery db view
  -> (   Pool PG.Connection
      -> Snap ()
      -> IO ())
  -- ^ low-level exposed DB details allows nice usage of 'Obelisk.Api.ReadDb' and 'Obelisk.Api.WriteDb'
  -> IO ()
withSimpleDbServer cfg handleRequest liveQuery k = withSimpleDbServerWithArg
  cfg
  (\() -> handleRequest)
  (mapLiveQuery (\f -> traverseSubVessel (\_ -> f)) liveQuery)
  (\dbConnPool serveApi -> k dbConnPool $ serveApi ())

redactUserInfo :: String -> String
redactUserInfo = \case
  "" -> ""
  _ -> "<redacted>@"

showConnectionString :: ByteString -> Text
showConnectionString bs = case fmap (parseURI . T.unpack) $ decodeUtf8' bs of
  Left _ -> "invalid unicode"
  Right Nothing -> "invalid URI"
  Right (Just uri) -> T.pack $ show $ uriToString redactUserInfo uri ""

withDbLogged :: forall db a. SimpleDbServerOptions db -> (ByteString -> Pool PG.Connection -> IO a) -> IO a
withDbLogged opts call =
  let myLog :: Text -> IO ()
      myLog = _simpleDbServerOptions_logger opts

      dbPath :: String
      dbPath = T.unpack $ _simpleDbServerOptions_dbPath opts
  in withDbUri dbPath $ \dbUri -> do
    myLog $ "database connection string: " <> showConnectionString dbUri
    withConnectionPool dbUri $ \dbConnPool -> do
      myLog "connected"
      call dbUri dbConnPool

withSimpleDbServerWithArg
  :: forall db request view arg
  .  _
  => SimpleDbServerConfig db
  -> (forall a. arg -> request a -> WriteDb a)
  -> LiveQuery db (SubVessel arg view)
  -> (   Pool PG.Connection
      -> (arg -> Snap ())
      -> IO ())
  -- ^ low-level exposed DB details allows nice usage of 'Obelisk.Api.ReadDb' and 'Obelisk.Api.WriteDb'
  -> IO ()
withSimpleDbServerWithArg cfg handleRequest view k = do
  let opts = _simpleDbServerConfig_options cfg
      myLog = _simpleDbServerOptions_logger opts
  withDbLogged opts $ \dbUri dbConnPool -> do
    let checkedDbSchema = _simpleDbServerConfig_schema cfg
        dbSchema = deAnnotateDatabase checkedDbSchema
        runDb :: forall a. WriteDb a -> IO a
        runDb = writeTransactionFromPool myLog dbConnPool
        requestHandler :: arg -> RequestHandler request IO
        requestHandler arg = RequestHandler $ \req -> runDb $ handleRequest arg req
    withResource dbConnPool $ migrateSimpleDb cfg
    serveDbOverWebsocketsNewWithArg @db @request
      myLog
      dbUri
      dbSchema
      requestHandler
      (\(QueryResultPatch d) q -> fmap (fmap IView . getMonoidalMap . getSubVessel . mapV (ResultV . runIdentity)) $ _liveQuery_listen view dbSchema d $ mapV (\_ -> Proxy) $ mkSubVessel $ MonoidalMap $ fmap getIView q)
      (\q -> fmap (fmap IView . getMonoidalMap . getSubVessel . mapV (ResultV . runIdentity)) $ _liveQuery_view view dbSchema $ mapV (\_ -> Proxy) $ mkSubVessel $ MonoidalMap $ fmap getIView q)
      (viewPipeline (\(Const ()) -> QueryV) (\(ResultV x) -> Identity x))
      $ \_serviceRegistrar serveApi -> k dbConnPool serveApi

migrateSimpleDb
  :: _
  => SimpleDbServerConfig db
  -> _
migrateSimpleDb cfg connection = do
  opts@(SimpleDbServerOptions
    { _simpleDbServerOptions_preMigration = preMigration
    , _simpleDbServerOptions_postMigration = postMigration
    })
    <- pure $ _simpleDbServerConfig_options cfg
  myLog <- pure $ _simpleDbServerOptions_logger opts
  myLog "migrating..."
  tryRunMigrationsWithEditUpdateAndHooks
    preMigration
    postMigration
    (_simpleDbServerOptions_editMigrationUpdates opts)
    (_simpleDbServerConfig_migrationSchema cfg $ _simpleDbServerConfig_schema cfg)
    connection
  myLog "migrated"

runSimpleDbTransaction
  :: forall db a
  .  _
  => SimpleDbServerConfig db
  -> WriteDb a
  -> IO a
runSimpleDbTransaction cfg k = do
  let opts = _simpleDbServerConfig_options cfg
  withDbLogged opts $ \_ dbConnPool -> do
    withResource dbConnPool $ migrateSimpleDb cfg
    let myLog = _simpleDbServerOptions_logger opts
    writeTransactionFromPool myLog dbConnPool k
