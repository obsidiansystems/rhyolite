module Obelisk.Db
  ( withDb
  , withDbUri
  , withDbUriOptions
  , withConnectionPool
  -- * Options
  , Options
  , options_extraPostgresConfig
  , defaultOptions
  ) where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Pool (Pool, createPool, destroyAllResources)
import Database.PostgreSQL.Simple as PG
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
      defaultConfig <- postgresNix
      let myConfig = defaultConfig
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
      withGargoyle myConfig dbPath a

openDbUri :: ByteString -> IO (Pool PG.Connection)
openDbUri dbUri = do
  let openPostgresql = connectPostgreSQL dbUri
      closePostgresql p = close p
  createPool openPostgresql closePostgresql 1 5 20 --TODO: Make the pool parameters configurable

withConnectionPool :: ByteString -> (Pool PG.Connection -> IO a) -> IO a
withConnectionPool dbUri = bracket (openDbUri dbUri) destroyAllResources
