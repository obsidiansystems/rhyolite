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

-- email
import qualified Rhyolite.Email

main :: IO ()
main = putStrLn "rhyolite submodules were successfully built"
