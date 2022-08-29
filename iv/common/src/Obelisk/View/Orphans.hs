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

instance (ArgDict c f, ArgDict c g) => ArgDict c (f :+: g) where
  type ConstraintsFor (f :+: g) c = (ConstraintsFor f c, ConstraintsFor g c)
  argDict (L1 f) = argDict f
  argDict (R1 g) = argDict g

instance (GShow f, GShow g) => GShow (f :+: g) where
  gshowsPrec n = \case
    L1 x -> showParen (n >= 11)
      $ showString "L1 "
      . gshowsPrec 11 x
    R1 x -> showParen (n >= 11)
      $ showString "R1 "
      . gshowsPrec 11 x

