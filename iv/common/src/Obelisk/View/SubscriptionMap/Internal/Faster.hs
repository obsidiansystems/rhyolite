{-# LANGUAGE UndecidableInstances #-}
module Obelisk.View.SubscriptionMap.Internal.Faster where

import Prelude hiding (pred)

import Control.Exception
import Control.Monad.State.Strict
import Obelisk.View.SubscriptionMap.Internal.Class
import Obelisk.View.Coverage
import Data.Align
import Data.These
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Witherable (catMaybes)
import Obelisk.View.CoverageMap
import Obelisk.View.Time
import Control.Lens.Indexed
import Obelisk.View.OccurrenceMap

import GHC.Stack

-- | Represents a range of Subscribed, Unsubscribed, and unknown but checkpointed.
--
-- Here is the cyclic transition diagram:
--
-- > Known Needed --> UnnSub --> Known Unneeded
-- >       ^  ^                   ^  ^
-- >        \  \                 /  /
-- >         \  Unused No Change   /
-- >          \         ^         /
-- >           \        |        /
-- >            \       |       /
-- >         Unknown (not in any map)
--
-- The "Known Unneeded" state means that either the time in question is unsubscribed or the subscription has already been fulfilled.
-- The "UnnSub" state exists only for points on the boundary between Known Unneeded and Unknown.  UnnSub is like Known Unneeded, except that if a NoChange is filled in in front of it, the NoChange becomes Subscribed, rather than Unsubscribed; the UnnSub is then "used up" and transitions to Known Unneeded.
--
-- It is a nice partial order. As such, we can consider the transitions to be monotonic by definition.
data SubscriptionMap a = SubscriptionMap
  { -- | known subscribed.
    _subscriptionMap_subscribed   :: CoverageMap a
  , -- | unneeded, but default subscribe
    _subscriptionMap_defaultSubscribe :: IntMap a -- Should only ever contain things within: topEdge (subscribed `union` unsubscribed)
  , -- | known unsubscribed.
    _subscriptionMap_unsubscribed :: FullBaseCoverageMap a
  , -- | checkpointed / no change. Will inherit from previous version (inductively) but not yet determined.
    _subscriptionMap_checkpointed :: CoverageMap (WithFullCoverage a)
  }
-- Laws:
-- * The needed, unneeded, and checkpointed regions are nonoverlapping
-- * Checkpointed space never immediately follows needed or unneeded space
-- * All points present in defaultSubscribe are in unneeded space immediately preceding unknown space (this results from needed space becoming unneeded)

isValidSubscriptionMap :: Coverage a => SubscriptionMap a -> Bool
isValidSubscriptionMap sm = and
  [ _subscriptionMap_subscribed sm `intersectionCoverageMaps` intMapToCoverageMap (_subscriptionMap_defaultSubscribe sm) == emptyCoverageMap
  ]

assertValidSubscriptionMap :: (HasCallStack, Coverage a) => SubscriptionMap a -> SubscriptionMap a
assertValidSubscriptionMap sm = assert (isValidSubscriptionMap sm) sm

mapSubscriptionMapMaybe :: (Coverage a, Coverage b) => (a -> Maybe b) -> (WithFullCoverage a -> Maybe (WithFullCoverage b)) -> SubscriptionMap a -> SubscriptionMap b
mapSubscriptionMapMaybe a2b af2bf xs = assertValidSubscriptionMap $ SubscriptionMap
  { _subscriptionMap_subscribed = mapCoverageMapMaybe a2b $ _subscriptionMap_subscribed xs
  , _subscriptionMap_defaultSubscribe = IntMap.mapMaybe a2b $ _subscriptionMap_defaultSubscribe xs
  , _subscriptionMap_unsubscribed = FullBaseCoverageMap
    { _fullBaseCoverageMap_base = mapCoverageMapMaybe af2bf $ _fullBaseCoverageMap_base $ _subscriptionMap_unsubscribed xs
    , _fullBaseCoverageMap_floating = mapCoverageMapMaybe a2b $ _fullBaseCoverageMap_floating $ _subscriptionMap_unsubscribed xs
    }
  , _subscriptionMap_checkpointed = mapCoverageMapMaybe af2bf $ _subscriptionMap_checkpointed xs
  }
{-# INLINE mapSubscriptionMapMaybe #-}

mapSubscriptionMap :: (Coverage a, Coverage b) => (a -> b) -> (WithFullCoverage a -> WithFullCoverage b) -> SubscriptionMap a -> SubscriptionMap b
mapSubscriptionMap a2b af2bf xs = assertValidSubscriptionMap $ SubscriptionMap
  { _subscriptionMap_subscribed = mapCoverageMap a2b $ _subscriptionMap_subscribed xs
  , _subscriptionMap_defaultSubscribe = a2b <$> _subscriptionMap_defaultSubscribe xs
  , _subscriptionMap_unsubscribed = FullBaseCoverageMap
    { _fullBaseCoverageMap_base = mapCoverageMap af2bf $ _fullBaseCoverageMap_base $ _subscriptionMap_unsubscribed xs
    , _fullBaseCoverageMap_floating = mapCoverageMap a2b $ _fullBaseCoverageMap_floating $ _subscriptionMap_unsubscribed xs
    }
  , _subscriptionMap_checkpointed = mapCoverageMap af2bf $ _subscriptionMap_checkpointed xs
  }
{-# INLINE mapSubscriptionMap #-}


-- Complete inputs:
-- * Version -> CovKey a -> Maybe (Maybe Bool) -- Whether each point is subscribed, unsubscribed, no change, or unknown
-- * Version -> CovKey a -> Maybe () -- Whether each point is fulfilled
-- Complete outputs for all time:
-- * Version -> CovKey a -> Maybe Bool -> Whether each point is needed, unneeded, or unknown
-- Storage:
-- * Pending no change :: CoverageMap (WithFullCoverage a) -- A time during which there won't be a subscribe or unsubscribe, but the immediately prior points aren't know to be either Needed or Unneeded.  When a point moves into Known Needed or Known Unneeded, it is removed from here.
-- * Pending needed :: CoverageMap a -- A time which will be needed unless an Unsubscribe shows up that gets rid of it.  When a point moves into Known Needed, it is removed from here.
-- * Known Needed :: CoverageMap a -- Either the actual time that was subscribed, or a contiguous block after it with NoChange, and which has not been fulfilled.  When a point is moved into Known unneeded, it is removed from here.
-- * Known Unneeded :: FullBaseCoverageMap a -- A time which is known to not be subscribed or which has been fulfilled
-- Overall: Version -> CovKey a -> White|Black|Red|Pink|Pink_Line|White_Line
-- Memory reclamation:
-- * "Known no change" will be 

-- | A CoverageMap whose data can be `WithFullCoverage a` whenever it's contiguous with minBound, but otherwise a
-- Laws:
--   * The base and floating portions do not overlap or touch at adjacent versions; i.e. forall t. lookupCoverageMap t base `intersectionCoverage` lookupCoverageMap (succ t) floating == Nothing
--   * The `base` value has strictly decreasing coverage from minBound up
data FullBaseCoverageMap a = FullBaseCoverageMap
  { _fullBaseCoverageMap_base :: CoverageMap (WithFullCoverage a)
  , _fullBaseCoverageMap_floating :: CoverageMap a
  }

emptyFullBaseCoverageMap :: FullBaseCoverageMap a
emptyFullBaseCoverageMap = FullBaseCoverageMap emptyCoverageMap emptyCoverageMap

fullBaseCoverageMapBaseOnly :: Coverage a => FullBaseCoverageMap (WithFullCoverage a) -> FullBaseCoverageMap a
fullBaseCoverageMapBaseOnly m = FullBaseCoverageMap (_fullBaseCoverageMap_base m) emptyCoverageMap

fullBaseCoverageMapToFullCoverageMap :: Coverage a => FullBaseCoverageMap a -> CoverageMap (WithFullCoverage a)
fullBaseCoverageMapToFullCoverageMap (FullBaseCoverageMap b f) = b `unionCoverageMaps` toWithFullCoverage f

fullBaseCoverageMapFromNonFull :: Coverage a => CoverageMap a -> FullBaseCoverageMap a
fullBaseCoverageMapFromNonFull = uncurry FullBaseCoverageMap . separateBaseFromFloating

-- | Separate the coverage that is representable as a nonincreasing smear from minBound ("base") from the coverage that is not ("floating")
separateBaseFromFloating :: forall a. Coverage a => CoverageMap a -> (CoverageMap (WithFullCoverage a), CoverageMap a)
separateBaseFromFloating m = unalignCoverageMap $ flip evalState (fmap toWithFullCoverage $ IntMap.lookup minBound $ unCoverageMap m) $ traverseFilterCoverageMapWithKey' g m
  where
    g :: Time -> Maybe a -> State (Maybe (WithFullCoverage a)) (Maybe (These (WithFullCoverage a) a))
    g _ currentIn = do
      oldBase <- get
      let newBase = oldBase `intersectionMaybeCoverage` toWithFullCoverage currentIn
      put newBase
      pure $ align newBase $ currentIn `differenceWithFullMaybeCoverage` newBase

intersectionFullBaseCoverageMap :: Coverage a => FullBaseCoverageMap a -> FullBaseCoverageMap a -> FullBaseCoverageMap a
intersectionFullBaseCoverageMap (FullBaseCoverageMap b1 f1) (FullBaseCoverageMap b2 f2) = FullBaseCoverageMap (b1 `intersectionCoverageMaps` b2) ((f1 `intersectionWithFullCoverageMaps` (b2 `unionCoverageMaps` toWithFullCoverage f2)) `unionCoverageMaps` (f2 `intersectionWithFullCoverageMaps` (b1 `unionCoverageMaps` toWithFullCoverage f1)))

unionFullBaseCoverageMap :: forall a. Coverage a => FullBaseCoverageMap a -> FullBaseCoverageMap a -> FullBaseCoverageMap a
unionFullBaseCoverageMap (FullBaseCoverageMap b1 f1) (FullBaseCoverageMap b2 f2) = FullBaseCoverageMap b f
  where
    bu = b1 `unionCoverageMaps` b2
    fu = f1 `unionCoverageMaps` f2
    g :: Maybe (WithFullCoverage a) -> Maybe (These (WithFullCoverage a) a) -> (Maybe (WithFullCoverage a), Maybe (These (WithFullCoverage a) a))
    g oldBase currentIn =
      let (mfa, ma) = unalign currentIn
          newBase = oldBase `intersectionMaybeCoverage` (toWithFullCoverage ma `unionMaybeCoverage` mfa)
          currentOut = ma `differenceWithFullMaybeCoverage` newBase
      in (newBase, align newBase currentOut)
    (b, f) = unalignCoverageMap $ snd $ mapAccumAlignFilterCoverageMaps g (Just fullCoverage) bu fu

deriving instance (Eq a, Eq (WithFullCoverage a)) => Eq (FullBaseCoverageMap a)
deriving instance (Show a, Show (WithFullCoverage a)) => Show (FullBaseCoverageMap a)

deriving instance (Eq a, Eq (WithFullCoverage a)) => Eq (SubscriptionMap a)
deriving instance (Show a, Show (WithFullCoverage a)) => Show (SubscriptionMap a)

limitToTopEdge :: Coverage a => CoverageMap a -> FullBaseCoverageMap a -> IntMap a -> IntMap a
limitToTopEdge sub unsub = catMaybes . IntMap.intersectionWith (flip intersectionWithFullCoverage) e
  where e = topEdge $ toWithFullCoverage sub `unionCoverage` fullBaseCoverageMapToFullCoverageMap unsub

emptySubscriptionMap' :: SubscriptionMap c
emptySubscriptionMap' = SubscriptionMap emptyCoverageMap IntMap.empty (FullBaseCoverageMap emptyCoverageMap emptyCoverageMap) emptyCoverageMap

instance SubscriptionMapC SubscriptionMap where
  emptySubscriptionMap = emptySubscriptionMap'

  subscribeSubscriptionMap
    :: ( HasCallStack
       , Coverage a
       , Show a
       , Show (WithFullCoverage a)
       )
    => Time -> a
    -> SubscriptionMap a
    -> (SubscriptionMap a, CoverageMap a)
  subscribeSubscriptionMap t c old =
    assert (singletonCoverageMap t c `intersectionWithFullCoverageMaps` fullBaseCoverageMapToFullCoverageMap (_subscriptionMap_unsubscribed old) == emptyCoverageMap) $
      ( assertValidSubscriptionMap $ old
        { _subscriptionMap_subscribed = newSubscribed
        , _subscriptionMap_defaultSubscribe = limitToTopEdge newSubscribed (_subscriptionMap_unsubscribed old) $ _subscriptionMap_defaultSubscribe old
        , _subscriptionMap_checkpointed = checkpointed'
        }
      , delta
      )
    where
      initDeltaSubscribed = singletonCoverageMap t c
      newSubscribed = sub' `unionCoverageMaps` _subscriptionMap_subscribed old
      -- We only need to have the newly endogenously subscribed as our "seed" /
      -- "thief", because any top edge of the old "known checkpointed" is
      -- already accounted for.
      -- TODO test the above
      (sub', checkpointed', delta) = rewardThief
        Nothing -- known ubsubscribed before epoch
        initDeltaSubscribed
        (_subscriptionMap_checkpointed old)

  fulfillSubscriptionMap cm old =
    assert (isValidSubscriptionMap old) $
    assert (_subscriptionMap_subscribed old `intersectionCoverageMaps` cm == cm) $
    assert (_fullBaseCoverageMap_base (_subscriptionMap_unsubscribed old) `intersectionCoverageMaps` toWithFullCoverage cm == emptyCoverageMap) $
    assert (_fullBaseCoverageMap_floating (_subscriptionMap_unsubscribed old) `intersectionCoverageMaps` cm == emptyCoverageMap) $
    let newUnsubscribed = _subscriptionMap_unsubscribed old `unionFullBaseCoverageMap` fullBaseCoverageMapFromNonFull (cm)
        newSubscribed = _subscriptionMap_subscribed old `differenceCoverageMaps` cm
         --TODO: Only check the parts of the edge that might have been affected by this particular subscribed area
    in assertValidSubscriptionMap $ old
    { _subscriptionMap_subscribed = newSubscribed
    , _subscriptionMap_defaultSubscribe = limitToTopEdge newSubscribed newUnsubscribed $ IntMap.unionWith unionCoverage (topEdge cm) $ _subscriptionMap_defaultSubscribe old
    , _subscriptionMap_unsubscribed = newUnsubscribed
    }

  unsubscribeSubscriptionMap t c old =
      ( assertValidSubscriptionMap $ old
        { _subscriptionMap_unsubscribed = newUnsubscribed
        , _subscriptionMap_defaultSubscribe = limitToTopEdge (_subscriptionMap_subscribed old) newUnsubscribed $ _subscriptionMap_defaultSubscribe old
        , _subscriptionMap_checkpointed = checkpointed'
        }
      , delta
      )
    where
      initDeltaUnsubscribed = singletonCoverageMap t c
      newUnsubscribed = fullBaseCoverageMapFromNonFull sub' `unionFullBaseCoverageMap` _subscriptionMap_unsubscribed old
      -- We only need to have the newly endogenously subscribed as our "seed" /
      -- "thief", because any top edge of the old "known checkpointed" is
      -- already accounted for.
      -- TODO test the above
      (sub', checkpointed', delta) = rewardThief
        (Just c) -- known ubsubscribed before epoch, other keys won't matter,
                 -- and we cannot 'Just fullCoverage' anyways
        initDeltaUnsubscribed
        (_subscriptionMap_checkpointed old)

  checkpointSubscriptions cm old = assertValidSubscriptionMap $ SubscriptionMap
      { _subscriptionMap_subscribed = newSubscribed
      , _subscriptionMap_defaultSubscribe = limitToTopEdge newSubscribed newUnsubscribed $ _subscriptionMap_defaultSubscribe old
      , _subscriptionMap_unsubscribed = newUnsubscribed
      , _subscriptionMap_checkpointed = newCheckpointed
      }
    where
      newUnsubscribed = FullBaseCoverageMap
        { _fullBaseCoverageMap_base = newUnsubscribedBase
        , _fullBaseCoverageMap_floating = newUnsubscribedFloating
        }
      -- | All the checkpointed points we know about; but some of them may be upgradable to `subscribed` or `unsubscribed` points
      allCheckpointed = _subscriptionMap_checkpointed old `unionCoverageMaps` cm
      phantomSubscriptions = intMapToCoverageMap (_subscriptionMap_defaultSubscribe old)
      (newSubscribedWithPhantoms, checkpointedWithoutSubscribed, _) = rewardThief Nothing (phantomSubscriptions `unionCoverageMaps` _subscriptionMap_subscribed old) allCheckpointed
      newSubscribed = newSubscribedWithPhantoms `differenceCoverageMaps` phantomSubscriptions
      (newUnsubscribedBase, checkpointedWithoutSubscribedAndUnsubscribedBase, stolenByUnsubscribedBase) = rewardThief (Just fullCoverage) (_fullBaseCoverageMap_base $ _subscriptionMap_unsubscribed old) $ toWithFullCoverage (_fullBaseCoverageMap_floating (_subscriptionMap_unsubscribed old)) `unionCoverageMaps` checkpointedWithoutSubscribed
      (newUnsubscribedFloating, newCheckpointed, _) = rewardThief Nothing (_fullBaseCoverageMap_floating (_subscriptionMap_unsubscribed old) `differenceWithFullCoverageMaps` stolenByUnsubscribedBase) (checkpointedWithoutSubscribedAndUnsubscribedBase `differenceCoverageMaps` toWithFullCoverage (_fullBaseCoverageMap_floating (_subscriptionMap_unsubscribed old))) -- We have to subtract the floating unsubscribeds out here because we added them in previously

  knownSubscriptions = _subscriptionMap_subscribed

  knownNonsubscriptions = fullBaseCoverageMapToFullCoverageMap . _subscriptionMap_unsubscribed

  unusedCheckpoints = _subscriptionMap_checkpointed

runSubscriptionMap
  :: (Applicative m, Coverage a)
  => (a -> Time -> m ()) -- ^ subscribe
  -> (a -> Time -> m ()) -- ^ unsubscribe
  -> (CoverageMap (WithFullCoverage a) -> m ()) -- ^ subscribeNone
  -> SubscriptionMap a -> m ()
runSubscriptionMap subscribe unsubscribe subscribeNone (SubscriptionMap subscribed defaultSubscribed (FullBaseCoverageMap base floating) checkpointed) =
  let
    toSubNone cov = cov `intersectionCoverageMaps` shiftCoverageMap 1 cov
    checkpoints =
      toWithFullCoverage (toSubNone subscribed)
      `unionCoverage` base
      `unionCoverage` toWithFullCoverage (toSubNone floating)
      `unionCoverage` checkpointed
    subs = bottomEdge subscribed
    unsubs = bottomEdge floating
    -- TODO:  i don't think defaultSubscribed is representable but failing to
    -- account for it would be inconsistent (either you expect a change you
    -- can't recieve, or else you eroneously switch to unsubscribed in the next
    -- timestep.
  in assert (IntMap.null defaultSubscribed)
   $ ifor_ subs (flip subscribe)
  *> ifor_ unsubs (flip unsubscribe)
  *> unless (nullCoverageMap checkpoints) (subscribeNone checkpoints)

subscribeSubscriptionMapOccurenceMap
  :: forall x. (Show x, Show (WithFullCoverage x), Coverage x)
  => OccurrenceMap x
  -> SubscriptionMap x
  -> SubscriptionMap x
subscribeSubscriptionMapOccurenceMap xs sm0 = ifoldl (\t sm x -> fst $ subscribeSubscriptionMap t x sm) sm0 (unOccurrenceMap xs)
unsubscribeSubscriptionMapOccurenceMap
  :: forall x. (Show x, Show (WithFullCoverage x), Coverage x)
  => OccurrenceMap x
  -> SubscriptionMap x
  -> SubscriptionMap x
unsubscribeSubscriptionMapOccurenceMap xs sm0 = ifoldl (\t sm x -> fst $ unsubscribeSubscriptionMap t x sm) sm0 (unOccurrenceMap xs)
