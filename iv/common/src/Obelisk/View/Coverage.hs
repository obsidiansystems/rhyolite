{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Obelisk.View.Coverage where

import Data.These
import Data.Bifunctor
import Obelisk.View.Misc
import Data.Functor.Const
import Data.Coerce
import Obelisk.View.DefMap
import Data.Map (Map)
import Data.Proxy
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntMap.Merge.Strict as IntMap
import Data.Functor.Compose
import Obelisk.View.These1
import Data.Void

class (Eq a, FullCoverage (WithFullCoverage a)) => Coverage a where
  -- | Every coverage type must have a corresponding type that can be "full". It may be the same type.
  type WithFullCoverage a :: *
  toWithFullCoverage :: a -> WithFullCoverage a
  default toWithFullCoverage :: (a ~ WithFullCoverage a) => a -> WithFullCoverage a
  toWithFullCoverage = id
  xorCoverage :: a -> a -> Maybe a
  unionCoverage :: a -> a -> a
  intersectionCoverage :: a -> a -> Maybe a
  differenceCoverage :: a -> a -> Maybe a
  -- There's no fullCoverage, because that may not be expressable for some coverage types (e.g. Set String) without using an infinite amount of memory
  intersectionWithFullCoverage :: a -> WithFullCoverage a -> Maybe a
  default intersectionWithFullCoverage :: (a ~ WithFullCoverage a) => a -> WithFullCoverage a -> Maybe a
  intersectionWithFullCoverage = intersectionCoverage
  differenceWithFullCoverage :: a -> WithFullCoverage a -> Maybe a
  default differenceWithFullCoverage :: (a ~ WithFullCoverage a) => a -> WithFullCoverage a -> Maybe a
  differenceWithFullCoverage = differenceCoverage

-- | Coverage that supports being "full"
class ( Coverage a
      , WithFullCoverage a ~ a -- Since this type can already be full, making it able to be full can't change it
      ) => FullCoverage a where
  -- | Law: forall a. a `intersectionCoverage` fullCoverage == a
  -- Law: forall a. a `unionCoverage` fullCoverage == fullCoverage
  -- Law: forall a. restrictCoverage fullCoverage a == a
  fullCoverage :: a

  negateCoverage :: a -> Maybe a
  negateCoverage = differenceCoverage fullCoverage

negateMaybeCoverage :: FullCoverage a => Maybe a -> Maybe a
negateMaybeCoverage = maybe (Just fullCoverage) negateCoverage

type family WithFullCoverage1 (f :: k -> *) :: k -> *
type instance WithFullCoverage1 (Compose f g) = Compose (WithFullCoverage1 f) g
type instance WithFullCoverage1 (Const a) = Const (WithFullCoverage a)
type instance WithFullCoverage1 Proxy = Proxy

class (WithFullCoverage (f a) ~ WithFullCoverage1 f a, Coverage (f a)) => CoverageFunctor f a

instance Eq (Compose (Const ()) f tbl) => CoverageFunctor (Compose (Const ()) f) tbl
instance Eq (Compose Proxy f tbl) => CoverageFunctor (Compose Proxy f) tbl
instance CoverageFunctor Proxy a

--------------------------------------------------------------------------------
-- Convenience functions
--------------------------------------------------------------------------------

xorMaybeCoverage :: Coverage a => Maybe a -> Maybe a -> Maybe a
xorMaybeCoverage ma mb = case (ma, mb) of
  (Nothing, Nothing) -> Nothing
  (Just a, Nothing) -> Just a
  (Nothing, Just b) -> Just b
  (Just a, Just b) -> xorCoverage a b

unionMaybeCoverage :: Coverage a => Maybe a -> Maybe a -> Maybe a
unionMaybeCoverage ma mb = case (ma, mb) of
  (Nothing, Nothing) -> Nothing
  (Just a, Nothing) -> Just a
  (Nothing, Just b) -> Just b
  (Just a, Just b) -> Just $ unionCoverage a b -- No need to check for empty, because if both were empty before they would already be Nothings

intersectionMaybeCoverage :: Coverage a => Maybe a -> Maybe a -> Maybe a
intersectionMaybeCoverage ma mb = case (ma, mb) of
  (Nothing, Nothing) -> Nothing
  (Just _, Nothing) -> Nothing
  (Nothing, Just _) -> Nothing
  (Just a, Just b) -> intersectionCoverage a b

intersectionWithFullMaybeCoverage :: Coverage a => Maybe a -> Maybe (WithFullCoverage a) -> Maybe a
intersectionWithFullMaybeCoverage ma mb = case (ma, mb) of
  (Nothing, Nothing) -> Nothing
  (Just _, Nothing) -> Nothing
  (Nothing, Just _) -> Nothing
  (Just a, Just b) -> intersectionWithFullCoverage a b

differenceMaybeCoverage :: Coverage a => Maybe a -> Maybe a -> Maybe a
differenceMaybeCoverage ma mb = case (ma, mb) of
  (Nothing, Nothing) -> Nothing
  (Just a, Nothing) -> Just a
  (Nothing, Just _) -> Nothing
  (Just a, Just b) -> differenceCoverage a b

differenceWithFullMaybeCoverage :: Coverage a => Maybe a -> Maybe (WithFullCoverage a) -> Maybe a
differenceWithFullMaybeCoverage ma mb = case (ma, mb) of
  (Nothing, Nothing) -> Nothing
  (Just a, Nothing) -> Just a
  (Nothing, Just _) -> Nothing
  (Just a, Just b) -> differenceWithFullCoverage a b

isSubCoverage :: Coverage a => a -> a -> Bool
isSubCoverage smaller larger = smaller `differenceCoverage` larger == Nothing

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

-- ()

instance Coverage () where
  type WithFullCoverage () = ()
  toWithFullCoverage = id
  xorCoverage () () = Nothing
  unionCoverage () () = ()
  intersectionCoverage () () = Just ()
  differenceCoverage () () = Nothing

instance FullCoverage () where
  fullCoverage = ()

-- Proxy

instance Coverage (Proxy a) where
  type WithFullCoverage (Proxy a) = Proxy a
  toWithFullCoverage = id
  xorCoverage Proxy Proxy = Nothing
  unionCoverage Proxy Proxy = Proxy
  intersectionCoverage Proxy Proxy = Just Proxy
  differenceCoverage Proxy Proxy = Nothing

instance FullCoverage (Proxy a) where
  fullCoverage = Proxy

-- Const

instance Coverage a => Coverage (Const a b) where
  type WithFullCoverage (Const a b) = Const (WithFullCoverage a) b
  toWithFullCoverage = coerce $ toWithFullCoverage @a
  xorCoverage = coerce $ xorCoverage @a
  unionCoverage = coerce $ unionCoverage @a
  intersectionCoverage = coerce $ intersectionCoverage @a
  differenceCoverage = coerce $ differenceCoverage @a
  intersectionWithFullCoverage = coerce $ intersectionWithFullCoverage @a
  differenceWithFullCoverage = coerce $ differenceWithFullCoverage @a

instance FullCoverage a => FullCoverage (Const a b) where
  fullCoverage = Const fullCoverage

-- These

instance (Coverage a, Coverage b) => Coverage (These a b) where
  type WithFullCoverage (These a b) = These (WithFullCoverage a) (WithFullCoverage b)
  toWithFullCoverage = bimap toWithFullCoverage toWithFullCoverage
  xorCoverage = combineTheseMaybe xorMaybeCoverage xorMaybeCoverage
  intersectionCoverage = combineTheseMaybe intersectionMaybeCoverage intersectionMaybeCoverage
  differenceCoverage = combineTheseMaybe differenceMaybeCoverage differenceMaybeCoverage
  unionCoverage = unionTheseWith unionCoverage unionCoverage
  intersectionWithFullCoverage = combineTheseMaybe intersectionWithFullMaybeCoverage intersectionWithFullMaybeCoverage
  differenceWithFullCoverage = combineTheseMaybe differenceWithFullMaybeCoverage differenceWithFullMaybeCoverage

instance (FullCoverage a, FullCoverage b) => FullCoverage (These a b) where
  fullCoverage = These fullCoverage fullCoverage

-- These1

instance
    ( CoverageFunctor f a , CoverageFunctor (WithFullCoverage1 f) a
    , CoverageFunctor g a , CoverageFunctor (WithFullCoverage1 g) a
    , Coverage a) => Coverage (These1 f g a) where
  type WithFullCoverage (These1 f g a) = These1 (WithFullCoverage1 f) (WithFullCoverage1 g) a
  toWithFullCoverage = bimapThese1 toWithFullCoverage toWithFullCoverage
  xorCoverage = combineThese1Maybe xorMaybeCoverage xorMaybeCoverage
  intersectionCoverage = combineThese1Maybe intersectionMaybeCoverage intersectionMaybeCoverage
  differenceCoverage = combineThese1Maybe differenceMaybeCoverage differenceMaybeCoverage
  unionCoverage = unionThese1With unionCoverage unionCoverage
  intersectionWithFullCoverage = combineThese1Maybe intersectionWithFullMaybeCoverage intersectionWithFullMaybeCoverage
  differenceWithFullCoverage = combineThese1Maybe differenceWithFullMaybeCoverage differenceWithFullMaybeCoverage

instance (CoverageFunctor f a, CoverageFunctor g a, Coverage a, FullCoverage (f a), FullCoverage (g a)) => FullCoverage (These1 f g a) where
  fullCoverage = These1 fullCoverage fullCoverage

-- DefMap

--TODO: DefMap k (Maybe v) can represent empty coverage; this shouldn't be possible, so we need something like a "nonempty DefMap"
instance (Ord k, Eq v, Coverage v, Coverage (WithFullCoverage v)) => Coverage (DefMap k (Maybe v)) where
  type WithFullCoverage (DefMap k (Maybe v)) = DefMap k (Maybe (WithFullCoverage v))
  toWithFullCoverage = mapDefMap $ fmap toWithFullCoverage
  xorCoverage a b = nothingEq (emptyDefMap Nothing) $ zipDefMapWith xorMaybeCoverage a b
  unionCoverage = zipDefMapWith unionMaybeCoverage
  intersectionCoverage a b = nothingEq (emptyDefMap Nothing) $ zipDefMapWith intersectionMaybeCoverage a b
  differenceCoverage a b = nothingEq (emptyDefMap Nothing) $ zipDefMapWith differenceMaybeCoverage a b
  intersectionWithFullCoverage a b = nothingEq (emptyDefMap Nothing) $ zipDefMapWith intersectionWithFullMaybeCoverage a b
  differenceWithFullCoverage a b = nothingEq (emptyDefMap Nothing) $ zipDefMapWith differenceWithFullMaybeCoverage a b

instance (Ord k, FullCoverage v) => FullCoverage (DefMap k (Maybe v)) where
  fullCoverage = emptyDefMap $ Just fullCoverage

-- Map

--TODO: Should really be NonEmptyMap
instance (Coverage v, Ord k) => Coverage (Map k v) where
  type WithFullCoverage (Map k v) = DefMap k (Maybe (WithFullCoverage v))
  toWithFullCoverage = DefMap Nothing . fmap (Just . toWithFullCoverage)
  xorCoverage a b = nothingIf Map.null $ Map.merge Map.preserveMissing Map.preserveMissing (Map.zipWithMaybeMatched $ \_ -> xorCoverage) a b
  unionCoverage = Map.unionWith unionCoverage
  intersectionCoverage a b = nothingIf Map.null $ Map.merge Map.dropMissing Map.dropMissing (Map.zipWithMaybeMatched $ \_ -> intersectionCoverage) a b
  differenceCoverage a b = nothingIf Map.null $ Map.merge Map.preserveMissing Map.dropMissing (Map.zipWithMaybeMatched $ \_ -> differenceCoverage) a b
  intersectionWithFullCoverage ma (DefMap mdb mmb) = nothingIf Map.null $ Map.merge whenBMissing whenAMissing whenBothPresent ma mmb
    where whenAMissing = Map.dropMissing
          whenBMissing = case mdb of
            Nothing -> Map.dropMissing
            Just db -> Map.mapMaybeMissing $ \_ a -> intersectionWithFullCoverage a db
          whenBothPresent = Map.zipWithMaybeMatched $ \_ a mb -> case mb of
            Nothing -> Nothing
            Just b -> intersectionWithFullCoverage a b
  differenceWithFullCoverage ma (DefMap mdb mmb) = nothingIf Map.null $ Map.merge whenBMissing whenAMissing whenBothPresent ma mmb
    where whenAMissing = Map.dropMissing
          whenBMissing = case mdb of
            Nothing -> Map.preserveMissing
            Just db -> Map.mapMaybeMissing $ \_ a -> differenceWithFullCoverage a db
          whenBothPresent = Map.zipWithMaybeMatched $ \_ a mb -> case mb of
            Nothing -> Just a
            Just b -> differenceWithFullCoverage a b

-- IntMap

--TODO: Should really be NonEmptyIntMap
instance Coverage v => Coverage (IntMap v) where
  type WithFullCoverage (IntMap v) = DefMap Int (Maybe (WithFullCoverage v))
  toWithFullCoverage = DefMap Nothing . fmap (Just . toWithFullCoverage) . intMapToMap
  xorCoverage a b = nothingIf IntMap.null $ IntMap.merge IntMap.preserveMissing IntMap.preserveMissing (IntMap.zipWithMaybeMatched $ \_ -> xorCoverage) a b
  unionCoverage = IntMap.unionWith unionCoverage
  intersectionCoverage a b = nothingIf IntMap.null $ IntMap.merge IntMap.dropMissing IntMap.dropMissing (IntMap.zipWithMaybeMatched $ \_ -> intersectionCoverage) a b
  differenceCoverage a b = nothingIf IntMap.null $ IntMap.merge IntMap.preserveMissing IntMap.dropMissing (IntMap.zipWithMaybeMatched $ \_ -> differenceCoverage) a b
  intersectionWithFullCoverage ma (DefMap mdb mmb) = nothingIf IntMap.null $ IntMap.merge whenBMissing whenAMissing whenBothPresent ma $ mapToIntMap mmb
    where whenAMissing = IntMap.dropMissing
          whenBMissing = case mdb of
            Nothing -> IntMap.dropMissing
            Just db -> IntMap.mapMaybeMissing $ \_ a -> intersectionWithFullCoverage a db
          whenBothPresent = IntMap.zipWithMaybeMatched $ \_ a mb -> case mb of
            Nothing -> Nothing
            Just b -> intersectionWithFullCoverage a b
  differenceWithFullCoverage ma (DefMap mdb mmb) = nothingIf IntMap.null $ IntMap.merge whenBMissing whenAMissing whenBothPresent ma $ mapToIntMap mmb
    where whenAMissing = IntMap.dropMissing
          whenBMissing = case mdb of
            Nothing -> IntMap.preserveMissing
            Just db -> IntMap.mapMaybeMissing $ \_ a -> differenceWithFullCoverage a db
          whenBothPresent = IntMap.zipWithMaybeMatched $ \_ a mb -> case mb of
            Nothing -> Just a
            Just b -> differenceWithFullCoverage a b

-- Compose

instance ( Eq (Compose f g x)
         , Coverage (f (g x))
         , WithFullCoverage1 (WithFullCoverage1 f) ~ WithFullCoverage1 f
         , FullCoverage (WithFullCoverage1 f (g x))
         , Eq (Compose (WithFullCoverage1 f) g x)
         , WithFullCoverage (f (g x)) ~ WithFullCoverage1 f (g x)
         ) => Coverage (Compose f g x) where
  type WithFullCoverage (Compose f g x) = Compose (WithFullCoverage1 f) g x
  toWithFullCoverage = coerce $ toWithFullCoverage @(f (g x))
  xorCoverage = coerce $ xorCoverage @(f (g x))
  unionCoverage = coerce $ unionCoverage @(f (g x))
  intersectionCoverage = coerce $ intersectionCoverage @(f (g x))
  differenceCoverage = coerce $ differenceCoverage @(f (g x))
  intersectionWithFullCoverage = coerce $ intersectionWithFullCoverage @(f (g x))
  differenceWithFullCoverage = coerce $ differenceWithFullCoverage @(f (g x))

instance (Eq (Compose f g x), FullCoverage (f (g x)), WithFullCoverage1 f ~ f) => FullCoverage (Compose f g x) where
  fullCoverage = Compose fullCoverage

-- Maybe
--TODO: Instances for Maybe should not exist; Coverage really always assumes it is NonEmpty, but Maybe can be empty

instance Coverage a => Coverage (Maybe a) where
  type WithFullCoverage (Maybe a) = Maybe (WithFullCoverage a)
  toWithFullCoverage = fmap toWithFullCoverage
  xorCoverage x y = Just <$> xorMaybeCoverage x y --TODO: Is this correct?
  unionCoverage x y = unionMaybeCoverage x y --TODO: Is this correct?
  intersectionCoverage x y = Just <$> intersectionMaybeCoverage x y --TODO: Is this correct?
  differenceCoverage x y = Just <$> differenceMaybeCoverage x y --TODO: Is this correct?
  intersectionWithFullCoverage x y = Just <$> intersectionWithFullMaybeCoverage x y --TODO: Is this correct?
  differenceWithFullCoverage x y = Just <$> differenceWithFullMaybeCoverage x y --TODO: Is this correct?

instance FullCoverage a => FullCoverage (Maybe a) where
  fullCoverage = Just fullCoverage

-- Void

instance Coverage Void where
  type WithFullCoverage Void = () --TODO: This doesn't make sense to me, but we have to be able to provide some value for `fullCoverage`
  toWithFullCoverage = absurd
  xorCoverage = absurd
  unionCoverage = absurd
  intersectionCoverage = absurd
  differenceCoverage = absurd
  intersectionWithFullCoverage = absurd
  differenceWithFullCoverage = absurd

--------------------------------------------------------------------------------
-- Monoids
--------------------------------------------------------------------------------

-- | Provides a Monoid instance where mappend is `unionMaybeCoverage`
newtype UnionMaybeCoverage a = UnionMaybeCoverage { unUnionMaybeCoverage :: Maybe a }
  deriving (Functor)

instance Coverage a => Semigroup (UnionMaybeCoverage a) where
  UnionMaybeCoverage a <> UnionMaybeCoverage b = UnionMaybeCoverage $ a `unionMaybeCoverage` b

instance Coverage a => Monoid (UnionMaybeCoverage a) where
  mempty = UnionMaybeCoverage Nothing

unionCoverages :: (Coverage a, Foldable f) => f a -> Maybe a
unionCoverages xs = unUnionMaybeCoverage $ foldMap (UnionMaybeCoverage . Just) xs

