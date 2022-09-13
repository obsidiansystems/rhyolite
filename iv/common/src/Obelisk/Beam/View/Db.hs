module Obelisk.Beam.View.Db where

import Obelisk.Beam.View.Table
import Obelisk.Beam.TablesV

newtype DbView db = DbView (TablesV db TableView)
