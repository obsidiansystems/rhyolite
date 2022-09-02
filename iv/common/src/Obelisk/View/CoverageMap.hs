module Obelisk.View.CoverageMap where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Obelisk.View.Coverage
import Obelisk.View.NonEmptyInterval
import Control.Monad.State.Strict (State)
import GHC.Stack
import Data.These
import Obelisk.View.Time
import Obelisk.View.Collidable
-- import Obelisk.View.IntIntervalMap
-- import qualified Obelisk.View.IntIntervalMap as IntIntervalMap
import Data.Witherable
import Control.Monad.Writer
import Control.Monad.State.Strict
import Obelisk.View.Coverable
import Obelisk.View.Misc
import Data.Foldable
import Control.Lens ((^?), _Just)
import Data.These.Lens
import Data.Maybe (isNothing, fromMaybe)
import Control.Exception (assert)
import Data.Tuple (swap)
import Data.Align
import Obelisk.View.DefMap
import qualified Data.Map.Strict as Map

-- Represents ranges of Bools over time and coverage type 'a'
newtype CoverageMap a = CoverageMap { unCoverageMap :: IntMap a } -- Each 'a' *toggles* the state of all the coverage it covers; initially, all coverage is False
  deriving (Eq, Show)

-- | Find all the covered points whose nearest neighbor above is not covered
topEdge :: Coverage a => CoverageMap a -> IntMap a
topEdge cm = coverageMapToIntMap $ cm `differenceCoverageMaps` shiftCoverageMap (-1) cm

-- | Add the given amount to all Times in the given CoverageMap
shiftCoverageMap :: Coverage a => Int -> CoverageMap a -> CoverageMap a
shiftCoverageMap n cm = case n `compare` 0 of
  EQ -> cm
  LT ->
    let pivot = minBound - n -- Everything before this is gone
        (dead, remaining) = splitLTIntMap pivot $ unCoverageMap cm
        deadCombined = foldl xorMaybeCoverage Nothing $ Just <$> dead
        remainingShifted = IntMap.mapKeysMonotonic (+ n) remaining
        endcap = foldl xorMaybeCoverage deadCombined $ Just <$> remainingShifted
    in CoverageMap $
       IntMap.alter (xorMaybeCoverage deadCombined) minBound $ -- Adjust the starting group to take into account the dead stuff
       maybe id (IntMap.insertWith (error "endcap shouldn't already exist") (maxBound + n + 1)) endcap $ -- Ensure the top end of the CoverageMap is fully empty
       remainingShifted
  GT ->
    let pivot = maxBound - n -- Everything after this is gone
        (remaining, _) = splitLEIntMap pivot $ unCoverageMap cm
        remainingShifted = IntMap.mapKeysMonotonic (+ n) remaining
    in CoverageMap remainingShifted

differenceIntMapCoverageMap :: Coverage a => IntMap a -> CoverageMap a -> IntMap a
differenceIntMapCoverageMap i c = IntMap.mapMaybeWithKey f i
  where f k a = Just a `differenceMaybeCoverage` lookupCoverageMap k c

intersectionIntMapCoverageMap :: Coverage a => IntMap a -> CoverageMap a -> IntMap a
intersectionIntMapCoverageMap i c = IntMap.mapMaybeWithKey f i
  where f k a = Just a `intersectionMaybeCoverage` lookupCoverageMap k c

-- | Find all the covered points whose nearest neighbor below is not covered
bottomEdge :: Coverage a => CoverageMap a -> IntMap a
bottomEdge cm = coverageMapToIntMap $ cm `differenceCoverageMaps` shiftCoverageMap 1 cm

-- | Find all the covered points whose nearest neighbor below is not covered
partitionBottomEdge :: Coverage a => CoverageMap a -> (IntMap a, CoverageMap a)
partitionBottomEdge cm = (coverageMapToIntMap $ cm `differenceCoverageMaps` shifted, cm `intersectionCoverageMaps` shifted)
  where
    shifted = shiftCoverageMap 1 cm

intMapToCoverageMap :: Coverage a => IntMap a -> CoverageMap a
intMapToCoverageMap m = CoverageMap $ catMaybes $ IntMap.unionWith xorMaybeCoverage m' $ IntMap.mapKeysMonotonic (+1) $ IntMap.delete maxBound m'
  where m' = Just <$> m

-- | Note: this can use extremely large amounts of memory if the given CoverageMap has large ranges of coverage
coverageMapToIntMap :: Coverage a => CoverageMap a -> IntMap a
coverageMapToIntMap cm = execWriter $ traverseWithInterval_CoverageMap f cm
  where f = \(NonEmptyInterval a b) c -> do
          forM_ [a..b] $ \t -> do
            tell $ IntMap.singleton t c

differenceCoverageMaps :: Coverage a => CoverageMap a -> CoverageMap a -> CoverageMap a
differenceCoverageMaps = zipCoverageMaps differenceMaybeCoverage

mapMaybeCoverageMap :: (Coverage a, Coverage b) => (a -> Maybe b) -> CoverageMap a -> CoverageMap b
mapMaybeCoverageMap f (CoverageMap changes) = CoverageMap $ IntMap.mapMaybe id $ evalState (traverseIntMapMaybeWithKeyInOrder (\_ -> fmap Just . update) changes) (Nothing, Nothing) --TODO: This fails if there are any negative keys
  where update change = do
          (oldCoverageA, oldCoverageB) <- get
          let newCoverageA = oldCoverageA `xorMaybeCoverage` Just change
              newCoverageB = f =<< newCoverageA
          put (newCoverageA, newCoverageB)
          pure $ oldCoverageB `xorMaybeCoverage` newCoverageB

--TODO: Foldable instance
--TODO: Most uses of this are probably bad; eliminate them
traverseWithInterval_CoverageMap
  :: forall a f
  . (Coverage a, Applicative f)
  => (NonEmptyInterval -> a -> f ())
  -> CoverageMap a -> f ()
traverseWithInterval_CoverageMap f (CoverageMap changes) =
    accum' *> runAt finalKey maxBound finalCoverage
  where
    -- NOTE: IntMap.traverseWithKey goes in the wrong key order; so we have to convert to list first instead
    -- TODO: Is this really true?
    (finalCoverage, finalKey, accum' :: f ())
      = execState (traverseIntMapWithKeyInOrder_ update changes) (Nothing, minBound, pure ())
    runAt start end coverage =
        -- nonEmptyIntervalEither can return Left in cases where minBound or maxBound are present in the map of changes
        for_ (nonEmptyIntervalEither start end) $ \r ->
          for_ coverage $ \c ->
            f r c
    update :: Int -> a -> State (Maybe a, Int, f ()) ()
    update newKey change = do
      (oldCoverage, oldKey, accum) <- get
      let newCoverage = oldCoverage `xorMaybeCoverage` Just change
      put ( newCoverage
          , newKey
          , accum *> when (newKey /= minBound) (runAt oldKey (pred newKey) oldCoverage)
          )

singletonCoverageMap :: Time -> a -> CoverageMap a
singletonCoverageMap t a = CoverageMap $
  (if t == maxBound then id else IntMap.insert (succ t) a) $
  IntMap.singleton t a

singletonRangeCoverageMap :: NonEmptyInterval -> a -> CoverageMap a
singletonRangeCoverageMap (NonEmptyInterval start end) a = CoverageMap $
  (if end == maxBound then id else IntMap.insert (succ end) a) $
  IntMap.singleton start a

-- | Given two disjoint coverage maps, combine adjacent regions where the
-- earlier is from the left map and the later is from the right map, and give it
-- to the earlier map. The inputted maps must be disjoint, and the outputted
-- maps are also disjoint.
--
-- stealHats l r = let
--    deltaL = { (v, a) | (v, a) in r && (v - 1, a) in l' } -- note '\'', recursive definition
--    l' = l \/ deltaL
--    r' = r \ l'
--  in deltaL
stealHats
  :: forall a
  .  ( Coverage a
     , HasCallStack
     , Show a
     , Show (WithFullCoverage a)
     )
  => Maybe a
  -> CoverageMap a
  -> CoverageMap (WithFullCoverage a)
  -> CoverageMap a
stealHats initThiefBag thief hatter = snd $ mapAccumAlignFilterCoverageMapsWithKey f Nothing thief hatter
  where
    f :: Time -> Maybe a -> Maybe (These a (WithFullCoverage a)) -> (Maybe a, Maybe a)
    f version oldVersionThief0 x = case
        oldTimeThief `intersectionWithFullMaybeCoverage` hats
      of
        Just e -> error $ "stealHats: not disjoint"
          <> unlines
            [ ":"
            , "thief = " <> show thief
            , "hatter = " <> show hatter
            , "oldTimeThief = " <> show oldTimeThief
            , "hats = " <> show hats
            , "oldTimeThief `intersectionWithFullMaybeCoverage` hats = " <> show e
            ]
        Nothing -> (currentVersionThief, canSteal)
      where
        oldVersionThief = if version == minBound
          then initThiefBag
          else oldVersionThief0
        oldTimeThief :: Maybe a
        hats :: Maybe (WithFullCoverage a)
        (oldTimeThief, hats) = unalign x
        canSteal = oldVersionThief `intersectionWithFullMaybeCoverage` hats
        currentVersionThief = canSteal `unionMaybeCoverage` oldTimeThief

-- | Thief -> Hatter -> (Thief', Hatter', Stolen Hats)
--
-- Idempotent:
--   twoPeople = (\(t,h,d) -> (t,h)) . uncurry rewardThief
--   twoPeople = twoPeople . twoPeople
--   -- TODO quickcheck
--
-- The thief knows his limits. If the hat is out of reach he won't grab it.
rewardThief
  :: ( Coverage a
     , HasCallStack
     , Show a
     , Show (WithFullCoverage a)
     )
  => Maybe a
  -> CoverageMap a
  -> CoverageMap (WithFullCoverage a)
  -> (CoverageMap a, CoverageMap (WithFullCoverage a), CoverageMap a)
rewardThief s thief hatter = ( thief `unionCoverageMaps` stolenHats
                           , hatter `differenceCoverageMaps` toWithFullCoverage stolenHats
                           , stolenHats
                           )
  where stolenHats = stealHats s thief hatter

-- Find the smallest time with any coverage, and the coverage at that time
lookupMinCoverageMap :: CoverageMap a -> Maybe (Time, a)
lookupMinCoverageMap (CoverageMap m) = IntMap.lookupMin m

-- | Return True if the given CoverageMap has no coverage for times less than the given time
isClearLTCoverageMap :: Time -> CoverageMap a -> Bool
isClearLTCoverageMap t (CoverageMap m) = isNothing $ IntMap.lookupLT t m

splitLEIntMap :: Int -> IntMap a -> (IntMap a, IntMap a)
splitLEIntMap t m = (maybe id (IntMap.insert t) at before, after)
  where (before, at, after) = IntMap.splitLookup t m

splitLTIntMap :: Int -> IntMap a -> (IntMap a, IntMap a)
splitLTIntMap t m = (before, maybe id (IntMap.insert t) at after)
  where (before, at, after) = IntMap.splitLookup t m

coverageMapFullCoveragesOnly :: FullCoverage a => CoverageMap a -> CoverageMap ()
coverageMapFullCoveragesOnly = mapCoverageMapMaybe $ \c ->
  if c == fullCoverage
  then Just ()
  else Nothing

negateCoverageMap :: FullCoverage a => CoverageMap a -> CoverageMap a
negateCoverageMap (CoverageMap m) = CoverageMap $ IntMap.alter f minBound m
  where f = xorMaybeCoverage (Just fullCoverage)

coverageMapEmptyCoveragesOnly :: Coverage a => CoverageMap a -> CoverageMap ()
coverageMapEmptyCoveragesOnly = negateCoverageMap . mapCoverageMap (\_ -> ())

emptyCoverageMap :: CoverageMap a
emptyCoverageMap = CoverageMap mempty

lookupCoverageMap
  :: Coverage a => Time -> CoverageMap a -> Maybe a
lookupCoverageMap t (CoverageMap changes) = foldl' xorMaybeCoverage beforeBigBang $
    fmap Just vals
  where
    (before, at, _) = IntMap.splitLookup t changes
    vals = maybe id (:) at $ IntMap.elems before
    beforeBigBang = Nothing

-- | CoverageMap has a coverage instance, so it "should" be NonEmpty; but empty
-- coverage is never-the-less representable.  assuming the map contains no
-- "empty" coverings, then the CoverageMap covers nothing only when it has no
-- elements.
nullCoverageMap :: CoverageMap a -> Bool
nullCoverageMap = IntMap.null . unCoverageMap
{-# INLINE nullCoverageMap #-}

-- | CoverageMap has a coverage instance, so it "should" be NonEmpty; but empty
-- coverage is never-the-less representable.  assuming the map contains no
-- "empty" coverings, then the CoverageMap covers nothing only when it has no
-- elements.
nonEmptyCoverageMap :: CoverageMap a -> Maybe (CoverageMap a)
nonEmptyCoverageMap xs = if nullCoverageMap xs then Nothing else Just xs
{-# INLINE nonEmptyCoverageMap #-}

-- | Adds a Time dimension
instance Coverage a => Coverage (CoverageMap a) where
  type WithFullCoverage (CoverageMap a) = CoverageMap (WithFullCoverage a)
  toWithFullCoverage = mapCoverageMap toWithFullCoverage
  unionCoverage = zipCoverageMaps unionMaybeCoverage
  intersectionCoverage a b = nothingEq emptyCoverageMap $ zipCoverageMaps intersectionMaybeCoverage a b
  xorCoverage a b = nothingEq emptyCoverageMap $ zipCoverageMaps xorMaybeCoverage a b
  differenceCoverage a b = nothingEq emptyCoverageMap $ zipCoverageMaps differenceMaybeCoverage a b
  intersectionWithFullCoverage a b = nothingEq emptyCoverageMap $ zipCoverageMaps intersectionWithFullMaybeCoverage a b
  differenceWithFullCoverage a b = nothingEq emptyCoverageMap $ zipCoverageMaps differenceWithFullMaybeCoverage a b

instance FullCoverage a => FullCoverage (CoverageMap a) where
  fullCoverage = singletonRangeCoverageMap allIntsNonEmptyInterval fullCoverage

mapCoverageMap
  :: forall a b
  .  ( Coverage a
     , Coverage b
     )
  => (a -> b)
  -> CoverageMap a
  -> CoverageMap b
mapCoverageMap f = mapCoverageMapMaybe $ Just . f

mapCoverageMapMaybe
  :: forall a b
  .  ( Coverage a
     , Coverage b
     )
  => (a -> Maybe b)
  -> CoverageMap a
  -> CoverageMap b
mapCoverageMapMaybe f (CoverageMap ma) = CoverageMap $ evalState (traverseIntMapMaybeWithKeyInOrder g ma) (Nothing, Nothing)
  where g :: Time -> a -> State (Maybe a, Maybe b) (Maybe b)
        g _ thisA = do
          (oldA :: Maybe a, oldB :: Maybe b) <- get
          let newA = oldA `xorMaybeCoverage` Just thisA
              newB = f =<< newA
              thisB = newB `xorMaybeCoverage` oldB
          put (newA, newB)
          return thisB

-- coverageMapToIntIntervalMap :: Coverage a => CoverageMap a -> IntIntervalMap a
-- coverageMapToIntIntervalMap (CoverageMap m) = IntIntervalMap.unsafeFromChanges $ flip evalState Nothing $ flip traverseIntMapMaybeWithKeyInOrder m $ \_ v -> do
--   old <- get
--   let new = old `xorMaybeCoverage` Just v
--   put new
--   pure $ Just new
-- 
-- intIntervalMapToCoverageMap :: Coverage a => IntIntervalMap a -> CoverageMap a
-- intIntervalMapToCoverageMap m = CoverageMap $ flip evalState Nothing $ flip traverseIntMapMaybeWithKeyInOrder (IntIntervalMap.toChanges m) $ \_ v -> do
--   old <- get
--   let new = old `xorMaybeCoverage` v
--   put v
--   pure new

newtype CoverageMapCollision a = CoverageMapCollision (CoverageMap a) deriving (Show, Eq)

instance Coverage a => Collidable (CoverageMap a) where
  type Collision (CoverageMap a) = CoverageMapCollision a
  mergeDisjoint a b = if i == emptyCoverageMap then Right u else Left $ CoverageMapCollision i
    where (u, i) = unionIntersectionCoverageMaps a b

unionCoverageMaps :: Coverage a => CoverageMap a -> CoverageMap a -> CoverageMap a
unionCoverageMaps = unionCoverage

unionCoverageMapsAssertDisjoint :: Coverage a => CoverageMap a -> CoverageMap a -> CoverageMap a
unionCoverageMapsAssertDisjoint a b =
  assert (a `intersectionCoverageMaps` b == emptyCoverageMap) $
  a `unionCoverageMaps` b

intersectionCoverageMaps :: Coverage a => CoverageMap a -> CoverageMap a -> CoverageMap a
intersectionCoverageMaps a b = snd $ unionIntersectionCoverageMaps a b

-- 'Maybe' is for cursors here, which have an additional position before or after.
-- We make 'Nothing' the position before. and 'Just n' the position after.
--
-- >    |   0    |   1    |   2    |   3    |  ...  | n - 1 |    n    |
-- > Nothing (Just 0) (Just 1) (Just 2) (Just 3)  ...  (Just n-1) (Just n)
lookupCoverageMapBoundary
  :: Coverage a => Maybe Time -> CoverageMap a -> Maybe a
lookupCoverageMapBoundary mt cm = case mt of
  Nothing -> beforeBigBang
  Just t  -> lookupCoverageMap t cm
  where beforeBigBang = Nothing

-- | Easier to use wrapper around '@alignFilterCoverageMaps@' that exploits 'Maybe
-- (These a b) ~~ (Maybe a, Maybe b)'.
zipCoverageMaps
  :: (Coverage a, Coverage b, Coverage c)
  => (Maybe a -> Maybe b -> Maybe c)
  -> CoverageMap a -> CoverageMap b -> CoverageMap c
zipCoverageMaps f = alignFilterCoverageMaps $
  \ newState -> f (newState ^? _Just . here) (newState ^? _Just . there)

-- | Ignores the old state for a regular "pure / non-path-dependent" align + filter
alignFilterCoverageMaps
  :: (Coverage a, Coverage b, Coverage c)
  => (Maybe (These a b) -> Maybe c)
  -> CoverageMap a -> CoverageMap b -> CoverageMap c
alignFilterCoverageMaps f = scanlAlignFilterCoverageMaps (const f)

-- | Takes the old state and the new state, for a "version history dependent"
-- align + filter The old state is the old *post aligned + filtered*, i.e. the
-- new map at the previous version, as opposed to the raw merged map state at
-- the previous version.
scanlAlignFilterCoverageMaps
  :: (Coverage a, Coverage b, Coverage c)
  => (Maybe c -> Maybe (These a b) -> Maybe c)
  -> CoverageMap a -> CoverageMap b -> CoverageMap c
scanlAlignFilterCoverageMaps f a b = snd $ mapAccumAlignFilterCoverageMaps
  (\s new -> let s' = f s new in (s', s'))
  Nothing a b

-- | Statefully align two coverage maps.
mapAccumAlignFilterCoverageMaps
  :: forall a b c s
  .  (Coverage a, Coverage b, Coverage c)
  => (s -> Maybe (These a b) -> (s, Maybe c))
  -> s
  -> CoverageMap a
  -> CoverageMap b
  -> (s, CoverageMap c)
mapAccumAlignFilterCoverageMaps f s0 a b = swap $ runState
  (traverseAlignFilterCoverageMaps' (\v -> state $ \s -> swap $ f s v) a b)
  s0

-- | Statefully align two coverage maps.
mapAccumAlignFilterCoverageMapsWithKey
  :: forall a b c s
  .  (Coverage a, Coverage b, Coverage c)
  => (Time -> s -> Maybe (These a b) -> (s, Maybe c))
  -> s
  -> CoverageMap a
  -> CoverageMap b
  -> (s, CoverageMap c)
mapAccumAlignFilterCoverageMapsWithKey f s0 a b = swap $ runState
  (traverseAlignFilterCoverageMapsWithKey' (\k v -> state $ \s -> swap $ f k s v) a b)
  s0

traverseAlignFilterCoverageMaps'
  :: forall f a b c
  .  (Coverage a, Coverage b, Coverage c, Monad f)
  => (Maybe (These a b) -> f (Maybe c))
  -> CoverageMap a
  -> CoverageMap b
  -> f (CoverageMap c)
traverseAlignFilterCoverageMaps' f = traverseAlignFilterCoverageMapsWithKey' $ const f

-- TODO cannot implement class because alignWith would need (Coverage c) constraint
alignCoverageMap
  :: CoverageMap a
  -> CoverageMap b
  -> CoverageMap (These a b)
alignCoverageMap (CoverageMap a) (CoverageMap b) = CoverageMap $ align a b

unalignCoverageMap
  :: CoverageMap (These a b)
  -> (CoverageMap a, CoverageMap b)
unalignCoverageMap (CoverageMap ab) = (CoverageMap ma, CoverageMap mb)
  where (ma, mb) = unalign ab

-- | Faster version with Monad
traverseAlignFilterCoverageMapsWithKey'
  :: forall f a b c
  .  (Coverage a, Coverage b, Coverage c, Monad f)
  => (Time -> Maybe (These a b) -> f (Maybe c))
  -> CoverageMap a
  -> CoverageMap b
  -> f (CoverageMap c)
traverseAlignFilterCoverageMapsWithKey' f a b =
  traverseFilterCoverageMapWithKey' f $ alignCoverageMap a b

-- | Faster version with Monad
traverseFilterCoverageMapWithKey'
  :: forall f a c
  .  (Coverage a, Coverage c, Monad f)
  => (Time -> Maybe a -> f (Maybe c))
  -> CoverageMap a
  -> f (CoverageMap c)
traverseFilterCoverageMapWithKey' f (CoverageMap t) = do
    (stated :: IntMap (Maybe c), _) <- runStateT (traverseIntMapWithKeyInOrder g t) (Nothing, Nothing)
    pure $ CoverageMap $ IntMap.mapMaybe id stated
  where
    g :: Time -> a -> StateT (Maybe a, (Maybe c)) f (Maybe c)
    g k v = do
      (previousInput, previousOutput) <- get
      let thisInput = previousInput `xorMaybeCoverage` Just v
      thisOutput <- lift $ f k thisInput
      put (thisInput, thisOutput)
      pure $ previousOutput `xorMaybeCoverage` thisOutput

--TODO: This has bad asymptotics: it always examines every key in the IntMap, even if the coverage region is small.  Instead, it should do some kind of recursive breakdown of both
restrictCoverageMap :: (Coverable a, Coverage (Cov a)) => CoverageMap (Cov a) -> IntMap a -> IntMap a
restrictCoverageMap cm = IntMap.mapMaybeWithKey $ \t v -> do
  c <- lookupCoverageMap t cm
  restrictCoverage c v

--TODO: See note on restrictCoverageMap
restrictWithFullCoverageMap :: (Coverable a, Coverage (Cov a)) => CoverageMap (WithFullCoverage (Cov a)) -> IntMap a -> IntMap a
restrictWithFullCoverageMap cm = IntMap.mapMaybeWithKey $ \t v -> do
  c <- lookupCoverageMap t cm
  restrictWithFullCoverage c v

intersectionWithFullCoverageMaps :: Coverage a => CoverageMap a -> CoverageMap (WithFullCoverage a) -> CoverageMap a
intersectionWithFullCoverageMaps = zipCoverageMaps intersectionWithFullMaybeCoverage

differenceWithFullCoverageMaps :: Coverage a => CoverageMap a -> CoverageMap (WithFullCoverage a) -> CoverageMap a
differenceWithFullCoverageMaps = zipCoverageMaps differenceWithFullMaybeCoverage

--TODO: Fix performance
unionIntersectionCoverageMaps :: Coverage a => CoverageMap a -> CoverageMap a -> (CoverageMap a, CoverageMap a)
unionIntersectionCoverageMaps a b = (unionCoverage a b, fromMaybe emptyCoverageMap $ intersectionCoverage a b)

instance Coverage a => Semigroup (CoverageMapCollision a) where
  CoverageMapCollision a <> CoverageMapCollision b = CoverageMapCollision $ fst $ unionIntersectionCoverageMaps a b

instance Coverage a => Monoid (CoverageMapCollision a) where
  mempty = CoverageMapCollision $ CoverageMap mempty
  mappend = (<>)

transposeCoverageMapDefMap
  :: forall k v
  .  ( Coverage (DefMap k v)
     , Ord k
     , Coverage v
     )
  => CoverageMap (DefMap k v)
  -> DefMap k (CoverageMap v)
transposeCoverageMapDefMap cm = DefMap defaults byKey
  where defaults = mapCoverageMap (\(DefMap d _) -> d) cm
        byKey = Map.fromListWithKey (\_ (CoverageMap a) (CoverageMap b) -> CoverageMap $ IntMap.unionWith (error "unexpected overlap") a b)
          [ (k, CoverageMap $ IntMap.singleton t c)
          | (t, DefMap _ m) <- IntMap.toList $ unCoverageMap cm
          , (k, c) <- Map.toList m
          ]

unionAllTimesCoverageMap :: Coverage a => CoverageMap a -> Maybe a
unionAllTimesCoverageMap = unUnionMaybeCoverage . foldMap (UnionMaybeCoverage . Just) . unCoverageMap
