module Obelisk.Db
  ( withDb
  , withDbUri
  , withDbUriOptions
  , withConnectionPool
  , postgresNixLogicalReplication
  , postgresNixLogicalReplicationOptions
  -- * Options
  , Options
  , options_extraPostgresConfig
  , defaultOptions
  , AllFieldsHave
  , useObeliskNamingConvention
  ) where

import Control.Exception (bracket)
import Control.Lens
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Constraint
import Data.Foldable
import qualified Data.List.NonEmpty as NonEmpty
import Data.Pool (Pool, createPool, destroyAllResources)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Database.Beam
import Database.Beam.Schema.Tables
import Database.PostgreSQL.Simple as PG hiding (fold)
import GHC.Generics
import Gargoyle
import Gargoyle.PostgreSQL.Nix (postgresNix)
import System.Directory (doesFileExist)
import System.FilePath

-- | Connects to the database using information at the given location
withDb
  :: FilePath
  -- ^ The path of either a 'Gargoyle'-wrapped Postgres database or a file
  -- containing the URL of a Postgres database.  If the path does not exist, it
  -- will be created with 'Gargoyle'.
  -> (Pool PG.Connection -> IO a)
  -- ^ An IO action to run while the database is open; the database will remain
  -- open until this action exits.
  -> IO a
withDb dbPath a = withDbUri dbPath $ \dbUri -> withConnectionPool dbUri a

data Options = Options
  { options_extraPostgresConfig :: [String]
    -- ^ Extra lines to add to @postgresql.conf@.
  }

defaultOptions :: Options
defaultOptions = Options
  { options_extraPostgresConfig = []
  }

withDbUri :: FilePath  -> (ByteString -> IO a)  -> IO a
withDbUri = withDbUriOptions defaultOptions

withDbUriOptions
  :: Options -- Use 'defaultOptions' for no additional options
  -> FilePath
  -> (ByteString -> IO a)
  -> IO a
withDbUriOptions options dbPath a = do
  dbExists <- doesFileExist dbPath
  if dbExists
    -- use the file contents as the uri for an existing server
    then do
      dbUri <- BS.readFile dbPath
      a dbUri
    -- otherwise assume its a folder for a local database
    else do
      myConfig <- postgresNixLogicalReplicationOptions options
      withGargoyle myConfig dbPath a

-- | A version of 'postgresNix' with logical replication enabled
postgresNixLogicalReplication :: IO (Gargoyle FilePath ByteString)
postgresNixLogicalReplication = postgresNixLogicalReplicationOptions defaultOptions

-- | A version of 'postgresNix' with logical replication enabled
postgresNixLogicalReplicationOptions :: Options -> IO (Gargoyle FilePath ByteString)
postgresNixLogicalReplicationOptions options = do
  defaultConfig <- postgresNix
  pure $ defaultConfig
        { _gargoyle_init = \workDir -> do
            _gargoyle_init defaultConfig workDir
            appendFile (workDir </> "postgresql.conf") $ unlines $
              [ ""
              , "# Added by Obelisk.Db"
              , "wal_level = logical"
              , "max_wal_senders = 10"
              ] ++ options_extraPostgresConfig options
                ++ [""]

            appendFile (workDir </> "pg_hba.conf") $ unlines
              [ ""
              , "# Added by Obelisk.Db"
              , "local replication postgres trust"
              , ""
              ]
        }

openDbUri :: ByteString -> IO (Pool PG.Connection)
openDbUri dbUri = do
  let openPostgresql = connectPostgreSQL dbUri
      closePostgresql p = close p
  createPool openPostgresql closePostgresql 1 5 20 --TODO: Make the pool parameters configurable

withConnectionPool :: ByteString -> (Pool PG.Connection -> IO a) -> IO a
withConnectionPool dbUri = bracket (openDbUri dbUri) destroyAllResources

--TODO: This should be moved to a more general location
type AllFieldsHave c a = GAllFieldsHave c (Rep a)

type family GAllFieldsHave c a :: Constraint where
  GAllFieldsHave c V1 = ()
  GAllFieldsHave c U1 = ()
  GAllFieldsHave c (a :+: b) = (GAllFieldsHave c a, GAllFieldsHave c b)
  GAllFieldsHave c (a :*: b) = (GAllFieldsHave c a, GAllFieldsHave c b)
  GAllFieldsHave c (K1 i a) = c a
  GAllFieldsHave c (M1 i t a) = GAllFieldsHave c a

-- | Assumes that field names are compatible with obelisk's field naming scheme
-- and creates significantly shorter names than the default would produce.
useObeliskNamingConvention
  :: forall be db
  . Database be db
  => DatabaseSettings be db
  -> DatabaseSettings be db
useObeliskNamingConvention d =
  runIdentity $ zipTables (Proxy @be) (\_ -> pure . f) d d
  where
    f :: forall e. IsDatabaseEntity be e => DatabaseEntity be db e -> DatabaseEntity be db e
    f (DatabaseEntity descriptor) =
      let
        renameEntity e = (dbEntityName %~ trimEntityName) e
        renameFields = withFieldRenamer (renamingFields $ fold . NonEmpty.intersperse (T.pack "_") . fmap trimFieldName)
      in
        DatabaseEntity (renameFields $ renameEntity descriptor)
    -- | Ideally this would work just like the field renaming works, but Beam
    -- doesn't keep the components of the entity name around like it does for
    -- fields; this will break if the user puts underscores inside entity or
    -- subdb names other than the two prescribed by the obelisk naming
    -- convention
    --
    -- Before this transformation is run, Beam will have created a table name
    -- that includes the names of the db datatypes, based on our naming
    -- convention.  For example, if we have a database type named C with a table
    -- _c_d inside of a database type named A in a field named _a_b :: C, then
    -- Beam will name the resulting table a_b__c_d.  However, we do not want the
    -- type names included, just the field names.  Therefore, we replace the
    -- double underscore (__) with a single one (_), and then strip out the
    -- even-numbered underscore-delimited portions, yielding b_d:
    --
    -- > trimEntityName "a_b__c_d" == "b_d"
    trimEntityName :: Text -> Text
    trimEntityName = T.intercalate "_" . dropEvens . T.splitOn "_" . T.replace "__" "_"
    trimFieldName :: Text -> Text
    trimFieldName = T.drop 1 . T.dropWhile (/= '_') . T.drop 1

dropEvens :: [a] -> [a]
dropEvens = \case
  [] -> []
  _:t -> dropOdds t

dropOdds :: [a] -> [a]
dropOdds = \case
  [] -> []
  h:t -> h : dropEvens t
