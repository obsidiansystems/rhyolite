{-# Language PackageImports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main where

--common
import Rhyolite.App
-- backend
import Rhyolite.Backend.App
-- frontend
import Rhyolite.Frontend.App
-- datastructures
import Rhyolite.SemiMap

-- psql-extras
import qualified Database.PostgreSQL.Simple.Class
import qualified Database.PostgreSQL.Simple.Beam
import qualified Database.PostgreSQL.Simple.Groundhog

-- notify-listen
import qualified Rhyolite.DB.NotifyListen
import qualified Rhyolite.DB.NotifyListen.Beam

-- groundhog-legacy
import qualified Rhyolite.DB.Groundhog

-- groundhog-legacy-types
import qualified "rhyolite-groundhog-legacy-types" Rhyolite.Account as RGLTAccount

-- email
import qualified Rhyolite.Email

-- signed-data
import qualified Data.Signed
-- signed-data-clientsession
import qualified Data.Signed.ClientSession

main :: IO ()
main = putStrLn "rhyolite submodules were successfully built"
