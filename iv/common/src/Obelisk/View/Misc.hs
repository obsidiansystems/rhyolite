-- | Functions that are used by, don't really have anything to do with, Obelisk.View.
-- Everything in this module should be upstreameed if there's a place where they fit
module Obelisk.View.Misc where

import Control.Lens ((^?))
import Data.These
import Data.These.Lens (here, there)
import Data.Functor.Identity
import Control.Applicative
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntMap.Merge.Strict as IntMap
import Data.Witherable
import Data.Tuple (swap)
import Control.Monad.State.Strict (runState, state)
import Data.Foldable
import Data.GADT.Compare
import Obelisk.View.These1
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Functor.Misc
import Data.Functor.Compose
import Data.Align

--------------------------------------------------------------------------------
-- Maybe
--------------------------------------------------------------------------------

nothingIf :: (a -> Bool) -> a -> Maybe a
nothingIf f a = if f a then Nothing else Just a

nothingIfNull :: Foldable f => f a -> Maybe (f a)
nothingIfNull a = if null a then Nothing else Just a

nothingEq :: Eq a => a -> a -> Maybe a
nothingEq zero = nothingIf (== zero)

--------------------------------------------------------------------------------
-- These
--------------------------------------------------------------------------------

unionTheseWith
  :: (a -> a -> a)
  -> (b -> b -> b)
  -> These a b
  -> These a b
  -> These a b
unionTheseWith opA opB = \case
  This xa -> \case
    This ya -> This $ opA xa ya
    That yb -> These xa yb
    These ya yb -> These (opA xa ya) yb
  That xb -> \case
    This ya -> These ya xb
    That yb -> That $ opB xb yb
    These ya yb -> These ya (opB xb yb)
  These xa xb -> \case
    This ya -> These (opA xa ya) xb
    That yb -> These xa (opB xb yb)
    These ya yb -> These (opA xa ya) (opB xb yb)

combineTheseMaybe
  :: (Maybe a -> Maybe a' -> Maybe a'')
  -> (Maybe b -> Maybe b' -> Maybe b'')
  -> These a b
  -> These a' b'
  -> Maybe (These a'' b'')
combineTheseMaybe opA opB x y = align
  (opA (x ^? here) (y ^? here))
  (opB (x ^? there) (y ^? there))

intersectionThese
  :: (a -> a' -> a'')
  -> (b -> b' -> b'')
  -> These a b
  -> These a' b'
  -> Maybe (These a'' b'')
intersectionThese f g = intersectionMaybeThese
  (\x y -> Just $ f x y)
  (\x y -> Just $ g x y)

intersectionMaybeThese
  :: (a -> a' -> Maybe a'')
  -> (b -> b' -> Maybe b'')
  -> These a b
  -> These a' b'
  -> Maybe (These a'' b'')
intersectionMaybeThese f g = \case
  This a -> \case
    This a' -> This <$> f a a'
    That _ -> Nothing
    These a' _ -> This <$> f a a'
  These a b -> \case
    This a' -> This <$> f a a'
    That b' -> That <$> g b b'
    These a' b' -> align (f a a') (g b b')
  That b -> \case
    This _ -> Nothing
    That b' -> That <$> g b b'
    These _ b' -> That <$> g b b'

mapMaybeThese
  :: (a -> Maybe a')
  -> (b -> Maybe b')
  -> These a b
  -> Maybe (These a' b')
mapMaybeThese f g = \case
  This a -> This <$> f a
  These a b -> align (f a) (g b)
  That b -> That <$> g b

alignWithThese
  :: (These a a' -> a'')
  -> (These b b' -> b'')
  -> These a b
  -> These a' b'
  -> These a'' b''
alignWithThese f g x y = runIdentity $ alignWithTheseA (Identity . f) (Identity . g) x y

alignWithTheseA
  :: Applicative f
  => (These a a' -> f a'')
  -> (These b b' -> f b'')
  -> These a b
  -> These a' b'
  -> f (These a'' b'')
alignWithTheseA f g = \case
  This a -> \case
    This a' -> This <$> f (These a a')
    That b' -> liftA2 These (f $ This a) (g $ That b')
    These a' b' -> liftA2 These (f $ These a a') (g $ That b')
  That b -> \case
    This a' -> liftA2 These (f $ That a') (g $ This b)
    That b' -> That <$> g (These b b')
    These a' b' -> liftA2 These (f $ That a') (g $ These b b')
  These a b -> \case
    This a' -> liftA2 These (f $ These a a') (g $ This b)
    That b' -> liftA2 These (f $ This a) (g $ These b b')
    These a' b' -> liftA2 These (f $ These a a') (g $ These b b')

alignWithMaybeThese
  :: (These a a' -> Maybe a'')
  -> (These b b' -> Maybe b'')
  -> These a b
  -> These a' b'
  -> Maybe (These a'' b'')
alignWithMaybeThese f g = \case
  This a -> \case
    This a' -> This <$> f (These a a')
    That b' -> align (f $ This a) (g $ That b')
    These a' b' -> align (f $ These a a') (g $ That b')
  That b -> \case
    This a' -> align (f $ That a') (g $ This b)
    That b' -> That <$> g (These b b')
    These a' b' -> align (f $ That a') (g $ These b b')
  These a b -> \case
    This a' -> align (f $ These a a') (g $ This b)
    That b' -> align (f $ This a) (g $ These b b')
    These a' b' -> align (f $ These a a') (g $ These b b')

mergeTheseA :: Applicative f => (a -> a -> f a) -> These a a -> f a
mergeTheseA f = \case
  This a -> pure a
  That b -> pure b
  These a b -> f a b

theseToMaybes :: These a b -> (Maybe a, Maybe b)
theseToMaybes t = (t ^? here, t ^? there)

--------------------------------------------------------------------------------
-- Map
--------------------------------------------------------------------------------

mergeMapDisjointWith :: Ord k => (a -> a -> Either c a) -> Map k a -> Map k a -> Either (Map k c) (Map k a)
mergeMapDisjointWith f a b = if Map.null bad then Right good else Left bad
  where (bad, good) = Map.mapEither id $ Map.merge (Map.mapMissing $ const Right) (Map.mapMissing $ const Right) (Map.zipWithMatched $ const f) a b

--------------------------------------------------------------------------------
-- IntMap
--------------------------------------------------------------------------------

intMapToMap :: IntMap v -> Map Int v
intMapToMap = Map.fromDistinctAscList . IntMap.toAscList

mapToIntMap :: Map Int v -> IntMap v
mapToIntMap = IntMap.fromDistinctAscList . Map.toAscList

--NOTE: Keys with empty IntMaps won't be preserved (is it even possible to preserve them?)
transposeMapIntMap :: Ord k => Map k (IntMap a) -> IntMap (Map k a)
transposeMapIntMap m = IntMap.fromListWith (Map.unionWith (error "overlap"))
  [ (i, Map.singleton k v)
  | (k, im) <- Map.toList m
  , (i, v) <- IntMap.toList im
  ]

--TODO: When we've upgraded to a sufficiently new `containers`, get rid of these
traverseIntMapMaybeWithKeyInOrder
  :: Applicative f
  => (IntMap.Key -> a -> f (Maybe b))
  -> IntMap a
  -> f (IntMap b)
traverseIntMapMaybeWithKeyInOrder f = fmap IntMap.fromDistinctAscList . wither (\(k, a) -> fmap ((,) k) <$> f k a) . IntMap.toAscList

traverseIntMapWithKeyInOrder
  :: Applicative f
  => (IntMap.Key -> a -> f b)
  -> IntMap a
  -> f (IntMap b)
traverseIntMapWithKeyInOrder f = fmap IntMap.fromDistinctAscList . traverse (\(k, a) -> ((,) k) <$> f k a) . IntMap.toAscList

mapAccumIntMapInOrder
  :: (s -> a -> (s, b)) -> s -> IntMap a -> (s, IntMap b)
mapAccumIntMapInOrder f s t = swap $ flip runState s $ traverseIntMapInOrder
  (\x -> state $ \s' -> swap $ f s' x)
  t

traverseIntMapInOrder
  :: Applicative f => (a -> f b) -> IntMap a -> f (IntMap b)
traverseIntMapInOrder f = traverseIntMapWithKeyInOrder (const f)

traverseIntMapWithKeyInOrder_
  :: Applicative f
  => (IntMap.Key -> a -> f ())
  -> IntMap a
  -> f ()
traverseIntMapWithKeyInOrder_ f = traverse_ (\(k, a) -> ((,) k) <$> f k a) . IntMap.toAscList

-- TODO: When we've upgraded to a sufficiently new `containers`, get rid of this
mergeAIntMapInOrder
  :: forall f a b c
  .  Applicative f
  => IntMap.WhenMissing f a c
  -> IntMap.WhenMissing f b c
  -> IntMap.WhenMatched f a b c
  -> IntMap a
  -> IntMap b
  -> f (IntMap c)
mergeAIntMapInOrder ac bc abc a b = traverseIntMapMaybeWithKeyInOrder (\_ x -> x) toSequence
  where
    toSequence :: IntMap (f (Maybe c))
    toSequence = IntMap.merge
      (IntMap.mapMissing $ IntMap.runWhenMissing ac)
      (IntMap.mapMissing $ IntMap.runWhenMissing bc)
      (IntMap.zipWithMatched $ IntMap.runWhenMatched abc)
      a
      b

--------------------------------------------------------------------------------
-- DMap
--------------------------------------------------------------------------------

--TODO: Create a more efficient implementation (e.g. don't rebuild the datastructure more than necessary)
alignDMapWithKey :: GCompare k => (forall a. k a -> These1 f g a -> h a) -> DMap k f -> DMap k g -> DMap k h
alignDMapWithKey f x y = DMap.mapWithKey f $ alignDMap x y

--TODO: Create a more efficient implementation (e.g. don't rebuild the datastructure more than necessary)
alignDMap :: GCompare k => DMap k f -> DMap k g -> DMap k (These1 f g)
alignDMap x y = DMap.unionWithKey (\_ (This1 f) (That1 g) -> These1 f g) (DMap.map This1 x) (DMap.map That1 y)

--TODO: Create a more efficient implementation (e.g. don't rebuild the datastructure more than necessary)
alignDMapWithKeyM :: (GCompare k, Applicative m) => (forall a. k a -> These1 f g a -> m (h a)) -> DMap k f -> DMap k g -> m (DMap k h)
alignDMapWithKeyM f x y = DMap.traverseWithKey f $ alignDMap x y

--TODO: Create a more efficient implementation (e.g. don't rebuild the datastructure more than necessary)
alignDMapMaybeWithKey :: GCompare k => (forall a. k a -> These1 f g a -> Maybe (h a)) -> DMap k f -> DMap k g -> DMap k h
alignDMapMaybeWithKey f x y = DMap.mapMaybeWithKey f $ alignDMap x y

intersectionDMapMaybeWithKey :: GCompare k => (forall x. k x -> a x -> b x -> Maybe (c x)) -> DMap k a -> DMap k b -> DMap k c
intersectionDMapMaybeWithKey f a b = DMap.mapMaybeWithKey (\_ -> getCompose) $ DMap.intersectionWithKey (\k x y -> Compose $ f k x y) a b

intersectionDMapWithKey :: GCompare k => (forall x. k x -> a x -> b x -> c x) -> DMap k a -> DMap k b -> DMap k c
intersectionDMapWithKey f = intersectionDMapMaybeWithKey $ \k a b -> Just $ f k a b

--------------------------------------------------------------------------------
-- ComposeMaybe
--------------------------------------------------------------------------------

mapComposeMaybe :: (f a -> g b) -> ComposeMaybe f a -> ComposeMaybe g b
mapComposeMaybe f (ComposeMaybe m) = ComposeMaybe $ fmap f m
