{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Rhyolite.Backend.Schema.Task
  ( module Rhyolite.Backend.Schema.Task
  , module Rhyolite.Schema.Task
  ) where

import Rhyolite.Schema.Task

import Database.Groundhog.TH
import Rhyolite.Backend.Schema.TH

mkRhyolitePersist Nothing [groundhog|
  - embedded: Task
|]
