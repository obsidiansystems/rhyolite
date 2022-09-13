{-# Language PackageImports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main where

--common
import Rhyolite.App
-- backend
import Rhyolite.Backend.App
-- frontend
import Rhyolite.Frontend.App
-- semimap
import Rhyolite.SemiMap
-- rhyolite-widgets
import qualified Reflex.Dom.Widget.ExtensibleList

-- psql-extras
import qualified Database.PostgreSQL.Simple.Class
import qualified Database.PostgreSQL.Simple.Beam
-- import qualified Database.PostgreSQL.Simple.Groundhog

-- notify-listen
import qualified Rhyolite.DB.NotifyListen
import qualified Rhyolite.DB.NotifyListen.Beam

-- -- groundhog-legacy
-- import qualified Rhyolite.DB.Groundhog

-- -- groundhog-legacy-types
-- import qualified "rhyolite-groundhog-legacy-types" Rhyolite.Account.Groundhog.Types

-- email
import qualified Rhyolite.Email

-- signed-data
import qualified Data.Signed
-- signed-data-clientsession
import qualified Data.Signed.ClientSession

-- accounts
import qualified "rhyolite-account-types" Rhyolite.Account
import qualified "rhyolite-account-backend" Rhyolite.Backend.Account

main :: IO ()
main = putStrLn "rhyolite submodules were successfully built"
