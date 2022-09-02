module Obelisk.View.SubscriptionMap.Internal.Class where

import Obelisk.View.Coverage
import Obelisk.View.Time
import Obelisk.View.CoverageMap
import Obelisk.View.NonEmptyInterval

import GHC.Stack

-- | dodgy class, but useful for testing. Internal Only.
class SubscriptionMapC (subscriptionMap :: * -> *) where

  emptySubscriptionMap
    :: Coverage a
    => subscriptionMap a

  -- | Subscribe at the given time, and produce a coverage map of anything that
  -- has moved into the Known Subscribed state
  subscribeSubscriptionMap
    :: ( HasCallStack
       , Coverage a
       , Show a
       , Show (WithFullCoverage a)
       )
    => Time
    -> a
    -> subscriptionMap a
    -> (subscriptionMap a, CoverageMap a)

  -- | Move some coverage from Subscribed to Unsubscribed; it must be fully present in the Subscribed region
  fulfillSubscriptionMap
    :: ( HasCallStack
       , Coverage a
       , Show a
       , Show (WithFullCoverage a)
       )
    => CoverageMap a
    -> subscriptionMap a
    -> subscriptionMap a

  --TODO: Rename "unsubscribe"
  unsubscribeSubscriptionMap
    :: ( HasCallStack
       , Coverage a
       , Show a
       , Show (WithFullCoverage a)
       )
    => Time
    -> a
    -> subscriptionMap a
    -> (subscriptionMap a, CoverageMap a)

  -- TODO: Probably return new known subscriptions
  checkpointSubscriptions
    :: ( HasCallStack
       , Coverage a
       , Show a
       , Show (WithFullCoverage a)
       )
    => CoverageMap (WithFullCoverage a)
    -> subscriptionMap a
    -> subscriptionMap a

  lookupKnownSubscription
    :: ( HasCallStack
       , Coverage a
       )
    => Time
    -> subscriptionMap a
    -> Maybe a
  lookupKnownSubscription t = lookupCoverageMap t . knownSubscriptions

  knownSubscriptions
    :: ( HasCallStack
       , Coverage a
       )
    => subscriptionMap a
    -> CoverageMap a
  knownSubscriptions = fst . knownSubscriptionStates

  knownNonsubscriptions
    :: ( HasCallStack
       , Coverage a
       )
    => subscriptionMap a
    -> CoverageMap (WithFullCoverage a)
  knownNonsubscriptions = snd . knownSubscriptionStates

  knownSubscriptionStates
    :: ( HasCallStack
       , Coverage a
       )
    => subscriptionMap a
    -> (CoverageMap a, CoverageMap (WithFullCoverage a))
  knownSubscriptionStates sm =
    ( knownSubscriptions sm
    , knownNonsubscriptions sm
    )

  unusedCheckpoints
    :: ( HasCallStack
       , Coverage a
       )
    => subscriptionMap a
    -> CoverageMap (WithFullCoverage a)

-- | This value is the unit of `unionSubscriptionMap`
allUnsubscribedSubscriptionMap :: (SubscriptionMapC a, Show c, Show (WithFullCoverage c), Coverage c) => a c
allUnsubscribedSubscriptionMap = checkpointSubscriptions (singletonRangeCoverageMap completeInterval fullCoverage) emptySubscriptionMap

fulfillSubscriptionMapFull
  :: (HasCallStack, Show a, Show (WithFullCoverage a), SubscriptionMapC subscriptionMap, Coverage a)
  => CoverageMap (WithFullCoverage a) -> subscriptionMap a -> subscriptionMap a
fulfillSubscriptionMapFull x xs =
  let
    wanted = knownSubscriptions xs
    expected = intersectionWithFullCoverage wanted x
    unexpected = differenceCoverage x (toWithFullCoverage wanted)
  in case unexpected of
    Nothing -> case expected of
      Just x' -> fulfillSubscriptionMap x' xs
      Nothing -> xs
    Just bad -> error $ "unexpected notification: " <> show bad
