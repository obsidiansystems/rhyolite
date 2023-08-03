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

-- notify-listen
import qualified Rhyolite.DB.NotifyListen
import qualified Rhyolite.DB.NotifyListen.Beam

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
