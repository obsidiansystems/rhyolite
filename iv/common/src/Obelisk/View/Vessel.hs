{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Werror=missing-methods #-}
module Obelisk.View.Vessel where

import Control.Applicative
import Data.Constraint.Extras
import Data.Constraint.Forall
import Data.Functor.Misc (ComposeMaybe(..))
import Data.These
import Data.Vessel.Class
import GHC.Generics
import Obelisk.View.Coverable
import Obelisk.View.Interface
import Obelisk.View.Coverage
import Obelisk.View.Collidable
import Obelisk.View.These1
import Reflex.Query.Class
import Data.Monoid (Sum(..))
import Data.Vessel.Map (MapV(..))

-- wrap a type with a View instance and derive the corresponding Coverage/Coverable/Collidable instances.
newtype IView v (f :: * -> *) = IView { getIView :: v f }
  deriving (Eq, Show, Semigroup)

class HasViewCov f where
  type ViewCov f :: * -> *
  coveredView :: f a -> ViewCov f a
  restrictCoverageView :: ViewCov f a -> f a -> Maybe (f a)

instance HasViewCov f => HasCov (IView v f) where
  type Cov (IView v f) = IView v (ViewCov f)

instance (View v, HasViewCov f) => Coverable (IView v f) where
  covered (IView a) = IView (mapV coveredView a)
  restrictCoverage (IView as) (IView bs) = IView <$> (mapMaybeV getComposeMaybe =<< (cropV (\a b -> ComposeMaybe $ restrictCoverageView a b) as bs))


either1 :: (f a -> b) -> (g a -> b) -> (f :+: g) a -> b
either1 f g = \case
  L1 l -> f l
  R1 r -> g r

class HasViewCollision f where
  type ViewCollision f :: * -> *
  mergeDisjointView :: f a -> f a -> (ViewCollision f :+: f) a

instance (HasViewCollision f, View v, Semigroup (v (ViewCollision f))) => Collidable (IView v f) where
  type Collision (IView v f) = IView v (ViewCollision f)
  mergeDisjoint (IView xs) (IView ys) =
    let xys = alignWithV (these R1 R1 mergeDisjointView) xs ys
        xs' = mapMaybeV (either1 Just (const Nothing)) xys
        ys' = mapMaybeV (either1 (const Nothing) Just) xys
    in case (xs', ys') of
      (Just bad, _) -> Left (IView bad)
      (Nothing, Just good) -> Right (IView good)
      (Nothing, Nothing) -> Right (IView xs) -- A bit weird here; we can't have *nothing* assuming the views were originally valid...  there were no errors so we just pick xs

instance forall v f. (ForallF Coverage f, View v, Eq (v f)) => Coverage (IView v f) where
  type WithFullCoverage (IView v f) = IViewFullCov v f
  toWithFullCoverage (IView x) = IViewFullCov False (collapseNullV x)
  intersectionWithFullCoverage x (IViewFullCov isFull ys) = case (isFull, ys) of
    (False, Nothing) -> Nothing
    (True, Nothing) -> Just x
    (False, Just y) -> intersectionCoverage x (IView y)
    (True, Just y)  -> differenceCoverage x (IView y)
  differenceWithFullCoverage x (IViewFullCov isFull ys) = case (isFull, ys) of
    (True, Nothing) -> Nothing
    (False, Nothing) -> Just x
    (True, Just y) -> intersectionCoverage x (IView y)
    (False, Just y) -> differenceCoverage x (IView y)
  intersectionCoverage (IView xs) (IView ys) = IView <$> alignWithMaybeV (these (const Nothing) (const Nothing) (\(x :: f x) -> whichever @Coverage @f @x (intersectionCoverage @(f x)) x)) xs ys
  differenceCoverage (IView xs) (IView ys) = IView <$> alignWithMaybeV (these Just (const Nothing) (\(x :: f x) -> whichever @Coverage @f @x (differenceCoverage @(f x)) x)) xs ys
  xorCoverage (IView xs) (IView ys) = IView <$> alignWithMaybeV (these Just Just (\(x :: f x) -> whichever @Coverage @f @x (xorCoverage @(f x)) x)) xs ys
  unionCoverage (IView xs) (IView ys) = IView $ alignWithV (these id id (\(x :: f x) -> whichever @Coverage @f @x (unionCoverage @(f x)) x)) xs ys

data IViewFullCov v (f :: * -> *) = IViewFullCov Bool (Maybe (v f))
  deriving (Eq, Show)

instance forall v f. (ForallF Coverage f, View v, Eq (v f)) => Coverage (IViewFullCov v f) where
  type WithFullCoverage (IViewFullCov v f) = IViewFullCov v f
  intersectionCoverage (IViewFullCov fx xs) (IViewFullCov fy ys) = case (fx, fy) of
    (False, False) -> IViewFullCov False . Just . getIView <$> intersectionMaybeCoverage (fmap IView xs) (fmap IView ys)
    (True, False) -> IViewFullCov False . Just . getIView <$> differenceMaybeCoverage (fmap IView ys) (fmap IView xs)
    (False, True) -> IViewFullCov False . Just . getIView <$> differenceMaybeCoverage (fmap IView xs) (fmap IView ys)
    (True, True) -> IViewFullCov True . Just . getIView <$> unionMaybeCoverage (fmap IView xs) (fmap IView ys)
  differenceCoverage (IViewFullCov fx xs) (IViewFullCov fy ys) = case (fx, fy) of
    (False, False) -> IViewFullCov False . Just . getIView <$> differenceMaybeCoverage (fmap IView xs) (fmap IView ys)
    (True, False) -> IViewFullCov True . Just . getIView <$> unionMaybeCoverage (fmap IView xs) (fmap IView ys)
    (False, True) -> IViewFullCov False . Just . getIView <$> intersectionMaybeCoverage (fmap IView xs) (fmap IView ys)
    (True, True) -> IViewFullCov False . Just . getIView <$> differenceMaybeCoverage (fmap IView ys) (fmap IView xs)
  unionCoverage (IViewFullCov fx xs) (IViewFullCov fy ys) = case (fx, fy) of
    (False, False) -> IViewFullCov False $ getIView <$> unionMaybeCoverage (fmap IView xs) (fmap IView ys)
    (True, False) -> IViewFullCov True $ getIView <$> differenceMaybeCoverage (fmap IView xs) (fmap IView ys)
    (False, True) -> IViewFullCov True $ getIView <$> differenceMaybeCoverage (fmap IView ys) (fmap IView xs)
    (True, True) -> IViewFullCov True $ getIView <$> intersectionMaybeCoverage (fmap IView xs) (fmap IView ys)
  xorCoverage (IViewFullCov fx xs) (IViewFullCov fy ys) = case fx /= fy of
    True -> IViewFullCov True . Just . getIView <$> xorMaybeCoverage (fmap IView xs) (fmap IView ys)
    False -> IViewFullCov False . Just . getIView <$> xorMaybeCoverage (fmap IView xs) (fmap IView ys)

instance (ForallF Coverage f, View v, Eq (v f)) => FullCoverage (IViewFullCov v f) where
  fullCoverage = IViewFullCov True Nothing

  negateCoverage (IViewFullCov True Nothing) = Nothing
  negateCoverage (IViewFullCov isFull x) = Just (IViewFullCov (not isFull) x)


-- | functors for use with Data.Vessel.View
data QueryV (a :: *) = QueryV
  deriving (Eq, Ord, Show, Read, Generic, Functor, Foldable, Traversable)

instance Applicative QueryV where
  _ <*> _ = QueryV
  pure _ = QueryV

instance Monad QueryV where
  _ >>= _ = QueryV

instance Semigroup (QueryV a) where
  _ <> _ = QueryV

instance Monoid (QueryV a) where
  mempty = QueryV

instance Coverage (QueryV a) where
  type WithFullCoverage (QueryV a) = QueryV a
  intersectionCoverage _ _ = Just QueryV
  differenceCoverage _ _ = Nothing
  unionCoverage _ _ = QueryV
  xorCoverage _ _ = Nothing

instance FullCoverage (QueryV a) where
  fullCoverage = QueryV

instance Monoid a => Query (QueryV a) where
  type QueryResult (QueryV a) = ResultV a
  crop _ = id

newtype ResultV a = ResultV { getResultV :: a }
  deriving (Eq, Ord, Show, Read, Generic, Functor, Foldable, Traversable)

instance Applicative ResultV where
  ResultV f <*> ResultV x = ResultV (f x)
  pure = ResultV
instance Monad ResultV where
  ResultV x >>= f = f x

instance Semigroup a => Semigroup (ResultV a) where
  (<>) = liftA2 (<>)
instance Monoid a => Monoid (ResultV a) where
  mempty = pure mempty

instance HasViewCov ResultV where
  type ViewCov ResultV = QueryV
  coveredView _ = QueryV
  restrictCoverageView _ = Just

instance HasViewCollision ResultV where
  type ViewCollision ResultV = Const (Sum Int)
  mergeDisjointView _ _ = L1 (Const 1)

instance (HasViewCov f, HasViewCov g) => HasViewCov (These1 f g) where
  type ViewCov (These1 f g) = These1 (ViewCov f) (ViewCov g)
  coveredView = bimapThese1 coveredView coveredView
  restrictCoverageView = combineThese1Maybe restrictMaybeCoverageView restrictMaybeCoverageView

restrictMaybeCoverageView :: HasViewCov f => Maybe (ViewCov f a) -> Maybe (f a) -> Maybe (f a)
restrictMaybeCoverageView mc ma = do
  c <- mc
  a <- ma
  restrictCoverageView c a

-- TODO; upstream this instead:
-- deriving instance (Ord k, Semigroup (f a)) => Semigroup (MapV k a f)
deriving instance (Ord k, Semigroup (ResultV a)) => Semigroup (MapV k a ResultV)

