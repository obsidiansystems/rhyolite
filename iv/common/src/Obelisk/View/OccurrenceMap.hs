{-# LANGUAGE UndecidableInstances #-}
module Obelisk.View.OccurrenceMap where

import Control.Lens (IxValue, Index, Iso, Ixed, iso, over, mapped, ifoldMap, preview, ix, At, at)
import qualified Data.Map.Monoidal as MMap -- TODO: Monoidal IntMap!
import Control.Applicative
import Control.Monad.Writer.CPS
import Control.Lens
import Data.Bifunctor (bimap)
import Obelisk.View.Time
import Obelisk.View.Collidable
import Obelisk.View.Coverage
import Obelisk.View.CoverageMap
import Data.Align
import Data.These
import Obelisk.View.Coverable
import Obelisk.View.NonEmptyInterval
import Data.Foldable

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import GHC.TypeLits

newtype OccurrenceMap a = OccurrenceMap
  { unOccurrenceMap :: IntMap a
  }
  deriving (Show, Eq, Foldable, Functor, Traversable)

type instance IxValue (OccurrenceMap a) = a
type instance Index (OccurrenceMap a) = Time

instance Ixed (OccurrenceMap a) where
  ix i f (OccurrenceMap m) = OccurrenceMap <$> ix i f m

instance TypeError ('Text "OccurrenceMap must not be empty, use ix") => At (OccurrenceMap a) where
  at _ _ _ = error "OccurrenceMap must not be empty"

-- TODO: instance TypeError ('Text "OccurrenceMap must not be empty, use ix") => Apply OccurrenceMap

intersectionWithOccurenceMap :: (a -> b -> c) -> OccurrenceMap a -> OccurrenceMap b -> Maybe (OccurrenceMap c)
intersectionWithOccurenceMap f (OccurrenceMap xs) (OccurrenceMap ys) = if null zs then Nothing else Just (OccurrenceMap zs)
  where
    zs = IntMap.intersectionWith f xs ys

intersectionWithMaybeOccurenceMap :: (a -> b -> c) -> Maybe (OccurrenceMap a) -> Maybe (OccurrenceMap b) -> Maybe (OccurrenceMap c)
intersectionWithMaybeOccurenceMap f xs ys = join $ liftA2 (intersectionWithOccurenceMap f) xs ys

instance FunctorWithIndex Time OccurrenceMap
instance FoldableWithIndex Time OccurrenceMap
instance TraversableWithIndex Time OccurrenceMap where
  itraverse f (OccurrenceMap xs) = OccurrenceMap <$> itraverse f xs

instance HasCov a => HasCov (OccurrenceMap a) where
  type Cov (OccurrenceMap a) = OccurrenceMap (Cov a)
instance Coverable a => Coverable (OccurrenceMap a) where
  covered (OccurrenceMap a) = OccurrenceMap $ fmap covered a
  restrictCoverage (OccurrenceMap cs) (OccurrenceMap xs) = mapMaybeOccurrenceMap id $ OccurrenceMap $ IntMap.intersectionWith restrictCoverage cs xs

instance Collidable a => Collidable (OccurrenceMap a) where
  type Collision (OccurrenceMap a) = MMap.MonoidalMap Time (Collision a)
  mergeDisjoint (OccurrenceMap a) (OccurrenceMap b) =
    bimap (MMap.fromList . IntMap.toList) OccurrenceMap $ mergeDisjoint a b

instance Coverage a => Coverage (OccurrenceMap a) where
  type WithFullCoverage (OccurrenceMap a) = CoverageMap (WithFullCoverage a)
  toWithFullCoverage (OccurrenceMap xs) = intMapToCoverageMap $ fmap toWithFullCoverage xs
  intersectionWithFullCoverage (OccurrenceMap xs) ys
    = mapMaybeOccurrenceMap id
    $ OccurrenceMap
    $ IntMap.intersectionWith intersectionWithFullCoverage xs
    $ intersectionIntMapCoverageMap (fmap toWithFullCoverage xs) ys

  differenceWithFullCoverage (OccurrenceMap xs) ys
    = mapMaybeOccurrenceMap id
    $ OccurrenceMap
    $ IntMap.intersectionWith intersectionWithFullCoverage xs
    $ differenceIntMapCoverageMap (fmap toWithFullCoverage xs) ys

  unionCoverage (OccurrenceMap xs) (OccurrenceMap ys) = OccurrenceMap $ IntMap.unionWith unionCoverage xs ys

  xorCoverage (OccurrenceMap xs) (OccurrenceMap ys) = mapMaybeOccurrenceMap id $ OccurrenceMap $ alignWith (these Just Just xorCoverage) xs ys

  intersectionCoverage (OccurrenceMap xs) (OccurrenceMap ys) = mapMaybeOccurrenceMap id $ OccurrenceMap $ IntMap.intersectionWith intersectionCoverage xs ys
  differenceCoverage (OccurrenceMap xs) (OccurrenceMap ys) = mapMaybeOccurrenceMap id $ OccurrenceMap $ alignWith (these Just (const Nothing) differenceCoverage) xs ys

instance Semigroup a => Semigroup (OccurrenceMap a) where
  OccurrenceMap xs <> OccurrenceMap ys = OccurrenceMap (IntMap.unionWith (<>) xs ys)

_OccurrenceMap :: Iso (OccurrenceMap a) (OccurrenceMap b) (IntMap a) (IntMap b)
_OccurrenceMap = iso unOccurrenceMap OccurrenceMap
{-# INLINE _OccurrenceMap #-}

zipOccurenceMapWithCoverageMap :: Coverage a => (a -> b -> Maybe c) -> CoverageMap a -> OccurrenceMap b -> Maybe (OccurrenceMap c)
zipOccurenceMapWithCoverageMap f ys (OccurrenceMap xs) = if null zs then Nothing else Just (OccurrenceMap zs)
  where
    -- the normally dodgy Monoid instance on IntMap is used conciously here;  traverseWithInterval_CoverageMap should not return overlapping intervals
    zs = execWriter $ traverseWithInterval_CoverageMap g ys
    g (NonEmptyInterval lb ub) cov = do
      let (_, lb', xs') = IntMap.splitLookup lb xs
          (xs'', ub', _) = IntMap.splitLookup ub xs'
      traverse_ (tell . IntMap.singleton lb) (lb' >>= f cov)
      tell (IntMap.mapMaybe (f cov) xs'')
      traverse_ (tell . IntMap.singleton ub) (ub' >>= f cov)
{-# INLINE zipOccurenceMapWithCoverageMap #-}

restrictOccurnceMapWithCoverageMap :: (Coverage (Cov a), Coverable a) => CoverageMap (Cov a) -> OccurrenceMap a -> Maybe (OccurrenceMap a)
restrictOccurnceMapWithCoverageMap = zipOccurenceMapWithCoverageMap restrictCoverage
{-# INLINE restrictOccurnceMapWithCoverageMap #-}

restrictMaybeOccurnceMapWithCoverageMap :: (Coverage (Cov a), Coverable a) => Maybe (CoverageMap (Cov a)) -> Maybe (OccurrenceMap a) -> Maybe (OccurrenceMap a)
restrictMaybeOccurnceMapWithCoverageMap x y = join $ liftA2 restrictOccurnceMapWithCoverageMap x y
{-# INLINE restrictMaybeOccurnceMapWithCoverageMap #-}

ifoldOccurrenceMap :: Monoid m => (Time -> a -> m) -> OccurrenceMap a -> m
ifoldOccurrenceMap f (OccurrenceMap m) = ifoldMap f m

mapOccurrenceMap :: (a -> b) -> OccurrenceMap a -> OccurrenceMap b
mapOccurrenceMap = over (_OccurrenceMap . mapped)
{-# INLINE mapOccurrenceMap #-}

mapMaybeOccurrenceMap :: (a -> Maybe b) -> OccurrenceMap a -> Maybe (OccurrenceMap b)
mapMaybeOccurrenceMap f (OccurrenceMap xs) =
  let xs' = IntMap.mapMaybe f xs
  in if IntMap.null xs' then Nothing else Just (OccurrenceMap xs')
{-# INLINE mapMaybeOccurrenceMap #-}

mapMaybeOccurrenceMapUnsafe :: (a -> Maybe b) -> OccurrenceMap a -> OccurrenceMap b
mapMaybeOccurrenceMapUnsafe f = over _OccurrenceMap (IntMap.mapMaybe f)
{-# INLINE mapMaybeOccurrenceMapUnsafe #-}

mapMaybeOccurrenceMapWithTime :: (Time -> a -> Maybe b) -> OccurrenceMap a -> OccurrenceMap b
mapMaybeOccurrenceMapWithTime f = over _OccurrenceMap (IntMap.mapMaybeWithKey f)
{-# INLINE mapMaybeOccurrenceMapWithTime #-}

singletonOccurrenceMap :: Time -> a -> OccurrenceMap a
singletonOccurrenceMap t a = OccurrenceMap $ IntMap.singleton t a

lookupOccurrenceMap :: Time -> OccurrenceMap a -> Maybe a
lookupOccurrenceMap t = preview (ix t)

unsafeEmptyOccurenceMap :: OccurrenceMap a
unsafeEmptyOccurenceMap = OccurrenceMap $ IntMap.empty

nullOccurenceMap :: OccurrenceMap a -> Bool
nullOccurenceMap (OccurrenceMap xs) = IntMap.null xs

occurrenceMap :: IntMap.IntMap a -> Maybe (OccurrenceMap a)
occurrenceMap xs = if IntMap.null xs then Nothing else Just (OccurrenceMap xs)

occurenceToCoverageMap :: Coverage a => OccurrenceMap a -> CoverageMap a
occurenceToCoverageMap (OccurrenceMap xs) = intMapToCoverageMap xs

traverseOccurenceMap_ :: forall a m. Applicative m => (a -> Time -> m ()) -> OccurrenceMap a -> m ()
traverseOccurenceMap_ f = itraverse_ (flip f)

occurenceMapFromList :: [(Time, a)] -> OccurrenceMap a
occurenceMapFromList = OccurrenceMap . IntMap.fromList
