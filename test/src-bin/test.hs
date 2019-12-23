{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main where

import Rhyolite.Aeson.Orphans -- aeson-orphans
import Rhyolite.App --common
import Rhyolite.Backend.App -- backend
import Rhyolite.Backend.DB -- backend-db
import Rhyolite.Backend.Snap.Util.FileServe.Stream -- backend-snap
import Rhyolite.Frontend.App -- frontend
import Rhyolite.SemiMap -- datastructures
import Rhyolite.Logging -- logging

main :: IO ()
main = putStrLn "rhyolite submodules were successfully built"
