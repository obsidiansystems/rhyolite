{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# language TypeOperators #-}
{-|
   Description:
     Types and helpers for Beam that can be used in schemas and database interactions.
-}
module Rhyolite.DB.Beam.Orphans where

import Data.Functor.Product
import Data.Proxy
import Database.Beam
import GHC.Generics

instance Beamable Proxy
instance (Beamable f, Beamable g) => Beamable (f :*: g)
instance (Beamable f, Beamable g) => Beamable (Product f g)
