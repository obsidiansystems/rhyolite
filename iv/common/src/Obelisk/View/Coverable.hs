{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Obelisk.View.Coverable where

import Control.Applicative
import Control.Lens ((^?))
import Control.Monad
import Data.Bifunctor
import Data.Constraint
import Data.Constraint.Forall
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Proxy
import Data.These
import Data.These.Lens
import Obelisk.View.Coverage
import Obelisk.View.These1
import Obelisk.View.Collidable
import Data.Map (Map)
import Data.IntMap (IntMap)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntMap.Merge.Strict as IntMap
import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.Functor.Misc
import Data.Align
import Data.Void

--------------------------------------------------------------------------------
-- Classes
--------------------------------------------------------------------------------

class HasCov (a :: k) where
  type Cov a :: k

--TODO: In the presence of sum types, there should really be 3 coverage types for a given datatype: coverage that can be requested (finite in all subsets), coverage that can be covered by the datatype (finite for actual data, but possibly infinite in excluded-data, e.g. the Left side of a Right-handed Either), and "full" coverage that can be infinite in any way.  We could also distinguish between the coverage types internally, but that eliminates our ability to do xor.
class HasCov a => Coverable a where
  covered :: a -> Cov a
  restrictCoverage :: Cov a -> a -> Maybe a
  -- Law: restrictCoverage c >=> restrictCoverage c === restrictCoverage c
  -- Law: forall c a. fmap covered (restrictCoverage c a) == covered a `intersectionCoverage` c

-- Alias classes
-- These classes function like newtypes, so that we can get arguments in the places we need them to be in

class (Cov (f a) ~ Cov f a) => CovFunctor f a

class (Cov (f a) ~ f (Cov a)) => CovFunctor' f a
instance (Cov (f a) ~ f (Cov a)) => CovFunctor' f a

class Forall (CovFunctor' v) => ForallCovFunctor' v
instance Forall (CovFunctor' v) => ForallCovFunctor' v

class (CovFunctor f a, Coverable (f a)) => CoverableFunctor f a
instance (CovFunctor f a, Coverable (f a)) => CoverableFunctor f a

--------------------------------------------------------------------------------
-- Convenience functions
--------------------------------------------------------------------------------

--TODO: Make this a method of Coverable so that it can be more performantly implemented
restrictWithFullCoverage :: (Coverable a, Coverage (Cov a)) => WithFullCoverage (Cov a) -> a -> Maybe a
restrictWithFullCoverage fc v = case covered v `intersectionWithFullCoverage` fc of
  Nothing -> Nothing
  Just c -> restrictCoverage c v

--TODO: Make this a method of Coverable so that it can be more performantly implemented
restrictDifferenceCoverage :: (Coverable a, Coverage (Cov a)) => Cov a -> a -> Maybe a
restrictDifferenceCoverage c v = case differenceCoverage (covered v) c of
  Nothing -> Nothing
  Just c' -> restrictCoverage c' v

restrictMaybeCoverage :: Coverable a => Maybe (Cov a) -> Maybe a -> Maybe a
restrictMaybeCoverage mc ma = do
  c <- mc
  a <- ma
  restrictCoverage c a

restrictMaybeWithFullCoverage :: (Coverable a, Coverage (Cov a)) => Maybe (WithFullCoverage (Cov a)) -> Maybe a -> Maybe a
restrictMaybeWithFullCoverage x y = join $ liftA2 restrictWithFullCoverage x y

covCommutes :: forall f a x. Forall (CovFunctor f) => (CovFunctor f a => x) -> x
covCommutes x = case inst @(CovFunctor f) @a of
  Sub Dict -> x

covCommutes' :: forall f a x. Forall (CovFunctor' f) => (CovFunctor' f a => x) -> x
covCommutes' x = case inst @(CovFunctor' f) @a of
  Sub Dict -> x

mergeOverwriting :: (Coverable a, Coverage (Cov a), Collidable a, Show (Collision a)) => a -> a -> a
mergeOverwriting winner loser = case covered loser `differenceCoverage` covered winner of
  Nothing -> winner
  Just toRetain -> mergeAssertDisjoint winner $ fromMaybe (error "mergeOverwriting: loser's coverage doesn't make sense") $ restrictCoverage toRetain loser

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

-- ()

instance HasCov () where
  type Cov () = ()

instance Coverable () where
  covered () = ()
  restrictCoverage () () = Just ()

-- Identity
instance HasCov Identity where
  type Cov Identity = Identity

instance HasCov (Identity a) where
  type Cov (Identity a) = Identity (Cov a)

deriving newtype instance Coverable a => Coverable (Identity a)

-- Compose

deriving instance HasCov (Compose f g a)
deriving instance Coverable (f (g a)) => Coverable (Compose f g a)

-- These

instance HasCov (These a b) where
  type Cov (These a b) = These (Cov a) (Cov b)

instance (Coverable a, Coverable b) => Coverable (These a b) where
  covered = bimap covered covered
  restrictCoverage c a = align
    (restrictMaybeCoverage (c ^? here) (a ^? here))
    (restrictMaybeCoverage (c ^? there) (a ^? there))

-- These1

instance (CovFunctor f a, CovFunctor g a) => HasCov (These1 f g a) where
  type Cov (These1 f g a) = These1 (Cov f) (Cov g) a

instance (CovFunctor f a, CovFunctor g a, Coverable (f a), Coverable (g a)) => Coverable (These1 f g a) where
  covered = bimapThese1 covered covered
  restrictCoverage c a = align1
    (restrictMaybeCoverage (justHere1 c) (justHere1 a))
    (restrictMaybeCoverage (justThere1 c) (justThere1 a))

-- Map

instance HasCov (Map k v) where
  type Cov (Map k v) = Map k (Cov v)

instance (Coverable v, Ord k) => Coverable (Map k v) where
  covered = fmap covered
  restrictCoverage p c =
    let m = Map.merge Map.dropMissing Map.dropMissing (Map.zipWithMaybeMatched $ \_ -> restrictCoverage) p c
    in if Map.null m then Nothing else Just m

-- IntMap

instance HasCov (IntMap v) where
  type Cov (IntMap v) = IntMap (Cov v)

instance (Coverable v) => Coverable (IntMap v) where
  covered = fmap covered
  restrictCoverage p c =
    let m = IntMap.merge IntMap.dropMissing IntMap.dropMissing (IntMap.zipWithMaybeMatched $ \_ -> restrictCoverage) p c
    in if IntMap.null m then Nothing else Just m

-- Const

instance HasCov (Const a) where
  type Cov (Const a) = Const (Cov a)

instance HasCov (Const a b) where
  type Cov (Const a b) = Const (Cov a) b

instance Coverable a => Coverable (Const a b) where
  covered (Const a) = Const $ covered a
  restrictCoverage (Const q) (Const a) = Const <$> restrictCoverage q a

-- Proxy

instance HasCov (Proxy) where
  type Cov Proxy = Proxy

instance HasCov (Proxy b) where
  type Cov (Proxy b) = Proxy b

instance Coverable a => Coverable (Proxy b) where
  covered Proxy = Proxy
  restrictCoverage Proxy Proxy = Just Proxy

-- Sum

instance HasCov (Sum Int) where
  type Cov (Sum Int) = ()

instance Coverable (Sum Int) where
  covered _ = ()
  restrictCoverage _ = Just

-- ComposeMaybe

instance HasCov (ComposeMaybe f) where
  type Cov (ComposeMaybe f) = ComposeMaybe (Cov f)

-- Void

instance HasCov Void where
  type Cov Void = Void

instance Coverable Void where
  covered = absurd
  restrictCoverage = absurd
