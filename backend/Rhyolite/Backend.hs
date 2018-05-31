module Rhyolite.Backend
    ( withDb
    ) where

import Rhyolite.Backend.DB

import Data.Pool (Pool)
import Database.Groundhog.Postgresql (Postgresql)
import System.Directory (doesFileExist)
import qualified Data.ByteString.Char8 as BS
import Gargoyle
import Gargoyle.PostgreSQL.Nix (postgresNix)

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
    then BS.readFile dbPath >>= openDb . head . BS.lines >>= a
    -- otherwise assume its a folder for a local database
    else do
      g <- postgresNix
      withGargoyle g dbPath $ \dbUri -> a =<< openDb dbUri
