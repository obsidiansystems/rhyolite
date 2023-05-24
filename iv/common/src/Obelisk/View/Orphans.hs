{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Obelisk.View.Orphans () where

import Data.Functor.Compose
import Database.Beam (Beamable)
import Data.Constraint.Extras
import Data.Monoid(Ap(..))
import GHC.Generics
import Data.GADT.Show

instance Beamable f => Beamable (Ap f)

deriving instance Eq (f (g a)) => Eq (Compose f g a)
deriving instance Show (f (g a)) => Show (Compose f g a)
