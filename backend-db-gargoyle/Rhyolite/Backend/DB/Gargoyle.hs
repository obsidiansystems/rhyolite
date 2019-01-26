module Rhyolite.Backend.DB.Gargoyle (withDb, openDb) where

import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Pool (Pool, createPool)
import Database.Groundhog.Postgresql (Postgresql (..))
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Gargoyle (withGargoyle)
import Gargoyle.PostgreSQL.Nix (postgresNix)
import System.Directory (doesFileExist)

-- | Connects to a database using information at the given filepath
-- The given filepath can be either a folder (for a local db)
-- or a file with a database url
--
-- withDb takes a String, which represents the path to a database, and a
-- function that returns database connection information as arguements in
-- order to open and start the database. Otherwise, it will create the
-- database for you if it doesn't exist.
withDb :: String -> (Pool Postgresql -> IO a) -> IO a
withDb dbPath a = do
  dbExists <- doesFileExist dbPath
  if dbExists
    -- use the file contents as the uri for an existing server
    then C8.readFile dbPath >>= openDb . head . C8.lines >>= a
    -- otherwise assume its a folder for a local database
    else do
      g <- postgresNix
      withGargoyle g dbPath $ openDb >=> a

openDb :: ByteString -> IO (Pool Postgresql)
openDb dbUri = do
  let openPostgresql = Postgresql <$> connectPostgreSQL dbUri
      closePostgresql (Postgresql p) = close p
  createPool openPostgresql closePostgresql 1 5 20
