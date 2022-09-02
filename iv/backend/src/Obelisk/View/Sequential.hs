{-# LANGUAGE UndecidableInstances #-}
module Obelisk.View.Sequential
  ( makeSequential
  , emptyIvForwardSequential
  , emptyIvBackwardSequential
  , IvForwardSequential (..)
  , IvBackwardSequential (..)
  , MakeSequentialState
  ) where

import Prelude hiding (id, (.))

import Obelisk.View.Misc
import Obelisk.View.Coverage
import Obelisk.View.Coverable

import Control.Monad
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Control.Category
import Control.Exception
import Data.Maybe
import Data.Align
import Data.These.Lens
import Control.Lens ((^?))
import Data.These
import Data.Bifunctor
-- import Obelisk.View.Iv.Class
import Obelisk.View.Collidable
import Obelisk.View.Time
import Obelisk.View.CoverageMap
import Obelisk.View.Interface
import Obelisk.View.Iv.Class
import Obelisk.View.NonEmptyInterval
import Control.Monad.Ref.Restricted
import Obelisk.View.IvMonad


-- | Adapt an Iv's inputs/outputs to work sequentially, such that:
--
-- * Incoming subscribes and unsubscribes always affect the latest time
-- * Incoming reads are always from the latest time
-- * Outgoing readResponses and notifies will always be in the order:
--   * readDone for time n
--   * notify for time n
--   * readDone for time (n+1)
--   * notify for time (n+1), etc.
-- * Outgoing readResponses and notifies will be sent in as they arrive, i.e. we will not wait for them to be complete.
--
makeSequential
  :: forall m a
  .  ( IvMonad m
     , Coverage (Cov (Push a))
     , Coverage (Cov (Pull a))
     , Coverable (Pull a)
     , Collidable (Pull a)
     , Show (Collision (Pull a))
     , Coverable (Push a)
     , Collidable (Push a)
     , Show (Collision (Push a))
     , RefData m (MakeSequentialState a)
     )
  => Time -- ^ Initial time
  -> IvForwardSequential m a
  -> IvBackward m a
  -> m ( IvForward m a
       , IvBackwardSequential m a
       -- Declare that all new requests should be made against the given time; this must
       -- always be > any previously-given time.  When any requests have been made at a
       -- given time, the system should try to update to a newer time as soon as possible.
       , Time -> m ()
       )
makeSequential t0 fs b = do

  state :: Ref m (MakeSequentialState a) <- newRef $ MakeSequentialState
    { _makeSequentialState_currentTime = t0
    , _makeSequentialState_pendingReads = IntMap.empty
    , _makeSequentialState_bufferedReads = IntMap.empty
    , _makeSequentialState_pendingNotifies = emptyCoverageMap
    , _makeSequentialState_bufferedNotifies = IntMap.empty
    , _makeSequentialState_sentSubscribes = Nothing
    , _makeSequentialState_pendingUnsubscribes = Nothing
    , _makeSequentialState_sentReads = Nothing
    , _makeSequentialState_currentReadResponses = Nothing
    }
  let advance old =
        let t = _makeSequentialState_currentTime old
            earliestReadsStillPossible = fromMaybe t $
              fmap fst $ IntMap.lookupMin $ _makeSequentialState_pendingReads old
            earliestNotifiesStillPossible = fromMaybe t $
              fmap fst $ lookupMinCoverageMap $ _makeSequentialState_pendingNotifies old
            -- We can send out reads for times <= earliestReadsStillWaiting AND <= earliestNotifiesStillWaiting
            latestReadTimeToSend = min earliestReadsStillPossible earliestNotifiesStillPossible
            (readsToSend, newBufferedReads) = splitLEIntMap latestReadTimeToSend $ _makeSequentialState_bufferedReads old
            -- We can send out notifications for times < earliestReadsStillWaiting AND <= earliestNotifesStillWaiting
            mLatestNotifyTimeToSend = if earliestReadsStillPossible == minBound
              then Nothing -- In this case, no notifications be sent out
              else Just $ min (pred earliestReadsStillPossible) earliestNotifiesStillPossible
            (notifiesToSend, newBufferedNotifies) = case mLatestNotifyTimeToSend of
              Nothing -> (IntMap.empty, _makeSequentialState_bufferedNotifies old)
              Just latestNotifyTimeToSend -> splitLEIntMap latestNotifyTimeToSend $ _makeSequentialState_bufferedNotifies old
        in ( old
             { _makeSequentialState_bufferedReads = newBufferedReads
             , _makeSequentialState_bufferedNotifies = newBufferedNotifies
             , _makeSequentialState_currentReadResponses = either (error . show) id $ _makeSequentialState_currentReadResponses old `mergeDisjoint` IntMap.lookup t readsToSend
             }
           , forM_ (align readsToSend notifiesToSend) $ \x -> do
               -- It's important that the readResponse come before the notify for each
               -- Time, because that's part of the interface of this function overall
               mapM_ (_ivForwardSequential_readResponse fs) $ x ^? here
               mapM_ (_ivForwardSequential_notify fs) $ x ^? there
           )

      updateStateAnd f = join $ atomicModifyRef' state $ \s0 ->
        let (s1, a1) = f s0
            (s2, a2) = advance s1
        in (s2, a1 >> a2)

      updateState f = updateStateAnd $ \old -> (f old, pure ())

  -- Determine if there are any prehistoric versions we have to establish coverage of.
  forM_ (lessThanInterval t0) $ \prehistory -> do
    -- Finalizing prehistoric subscriptions
    _ivBackward_subscribeNone b $ singletonRangeCoverageMap prehistory fullCoverage
    -- Finalizing prehistoric reads
    _ivBackward_readNone b $ singletonRangeCoverageMap prehistory fullCoverage
    -- Done with prehistoric finalizations

  pure
    ( IvForward
      { _ivForward_notify = \p t -> updateState $ \old -> old
        { _makeSequentialState_pendingNotifies = _makeSequentialState_pendingNotifies old `differenceCoverageMaps` singletonCoverageMap t (toWithFullCoverage $ covered p)
        , _makeSequentialState_bufferedNotifies = IntMap.insertWith (\x y -> either (error . show) id $ mergeDisjoint x y) t p $ _makeSequentialState_bufferedNotifies old
        }
      , _ivForward_notifyNone = \q -> updateState $ \old -> old
        { _makeSequentialState_pendingNotifies = _makeSequentialState_pendingNotifies old `differenceCoverageMaps` q
        }
        --TODO: we shouldn't actually advance past a time until we know that we won't receive any more subscriptions for that time
      , _ivForward_readResponse = \v t -> updateState $ \old -> old
        { _makeSequentialState_pendingReads = IntMap.alter (`differenceMaybeCoverage` Just (covered v)) t $ _makeSequentialState_pendingReads old --TODO: Ensure that this removes exactly the expected coverage from the given time (and that the given time is present in the map); otherwise, something has gone wrong
        , _makeSequentialState_bufferedReads = IntMap.insertWith (\x y -> either (error . show) id $ mergeDisjoint x y) t v $ _makeSequentialState_bufferedReads old
        }
      }
    , IvBackwardSequential
      { _ivBackwardSequential_subscribeUnsubscribeRead = \q -> updateStateAnd $ \old ->
          let t = _makeSequentialState_currentTime old
              ((mqSubscribe, mqUnsubscribe), mqRead) = first unalign $ theseToMaybes q
              mToSend = mqSubscribe `differenceMaybeCoverage` _makeSequentialState_sentSubscribes old
              toRequest = mqRead `differenceMaybeCoverage` _makeSequentialState_sentReads old
              toRespond = restrictMaybeCoverage mqRead . Just =<< _makeSequentialState_currentReadResponses old
          in ( old
               { _makeSequentialState_pendingNotifies = (_makeSequentialState_pendingNotifies old `unionCoverageMaps` maybe emptyCoverageMap (singletonRangeCoverageMap (greaterThanOrEqualInterval t) . toWithFullCoverage) mqSubscribe) `differenceCoverageMaps` maybe emptyCoverageMap (singletonRangeCoverageMap (greaterThanOrEqualInterval t) . toWithFullCoverage) mqUnsubscribe --TODO: Maybe we shouldn't actually expect all these notifications to be pending until time has advanced past them so we know they're locked in
               , _makeSequentialState_sentSubscribes = _makeSequentialState_sentSubscribes old `unionMaybeCoverage` mqSubscribe
               , _makeSequentialState_pendingUnsubscribes = (_makeSequentialState_pendingUnsubscribes old `differenceMaybeCoverage` mqSubscribe) `unionMaybeCoverage` mqUnsubscribe
               , _makeSequentialState_pendingReads = IntMap.alter (`unionMaybeCoverage` (mqRead `differenceMaybeCoverage` fmap covered toRespond)) t $ _makeSequentialState_pendingReads old
               , _makeSequentialState_sentReads = _makeSequentialState_sentReads old `unionMaybeCoverage` mqRead
               }
             , do
                 forM_ mToSend $ \toSend -> do
                   _ivBackward_subscribe b toSend t
                 forM_ toRequest $ \c -> do
                   _ivBackward_read b c t
                 forM_ toRespond $ \r -> do
                   _ivForwardSequential_readResponse fs r
             )
      }
    , \newTime -> updateStateAnd $ \old ->
      let oldTime = _makeSequentialState_currentTime old
          toUnsubscribeAtOldTime = _makeSequentialState_pendingUnsubscribes old `differenceMaybeCoverage` _makeSequentialState_sentSubscribes old
          toUnsubscribeAfterOldTime = _makeSequentialState_pendingUnsubscribes old `intersectionMaybeCoverage` _makeSequentialState_sentSubscribes old
          (toUnsubscribeAtSuccOldTime, unsubscribeStillPending) =
            if newTime > succ oldTime
            then (toUnsubscribeAfterOldTime, Nothing)
            else (Nothing, toUnsubscribeAfterOldTime)
          toReadNone = Just fullCoverage `differenceMaybeCoverage` toWithFullCoverage (_makeSequentialState_sentReads old)
          toSubscribeNone = Just fullCoverage `differenceMaybeCoverage` toWithFullCoverage (_makeSequentialState_sentSubscribes old `unionMaybeCoverage` toUnsubscribeAtOldTime)
      in assert (succ oldTime == newTime) $ --TODO: Support jumping by more than one time.  This will require adding logic to, e.g., readNone and subscribeNone all the intervening timesteps
         ( old
           { _makeSequentialState_currentTime = newTime
           , _makeSequentialState_sentSubscribes = Nothing
           , _makeSequentialState_pendingUnsubscribes = unsubscribeStillPending
           , _makeSequentialState_sentReads = Nothing
           , _makeSequentialState_currentReadResponses = Nothing
           }
         , do
             forM_ toUnsubscribeAtOldTime $ \c -> do
               _ivBackward_unsubscribe b c oldTime
             forM_ toUnsubscribeAtSuccOldTime $ \c -> do
               _ivBackward_unsubscribe b c $ succ oldTime
             forM_ toReadNone $ \c -> do
               _ivBackward_readNone b $ singletonCoverageMap oldTime c
             forM_ toSubscribeNone $ \c -> do
               _ivBackward_subscribeNone b $ singletonCoverageMap oldTime c
         )
    )

data MakeSequentialState a = MakeSequentialState
  { _makeSequentialState_currentTime :: !Time
    -- ^ The time at which new IvBackwardSequential requests will be effective
  , _makeSequentialState_pendingReads :: !(IntMap (Cov (Pull a)))
  , _makeSequentialState_bufferedReads :: !(IntMap (Pull a))
  , _makeSequentialState_pendingNotifies :: !(CoverageMap (WithFullCoverage (Cov (Push a))))
  , _makeSequentialState_bufferedNotifies :: !(IntMap (Push a))
  , _makeSequentialState_sentSubscribes :: !(Maybe (Cov (Push a)))
    -- ^ Subscribes that have been sent at the current time
  , _makeSequentialState_pendingUnsubscribes :: !(Maybe (Cov (Push a)))
    -- ^ Unsubscribes that need to be sent
  , _makeSequentialState_sentReads :: !(Maybe (Cov (Pull a))) --TODO: This seems somewhat redundant with pendingReads
  , _makeSequentialState_currentReadResponses :: !(Maybe (Pull a)) --TODO: This seems somewhat redundant with bufferedReads
  }

deriving instance
  ( Show (Cov (Pull a))
  , Show (Pull a)
  , Show (WithFullCoverage (Cov (Push a)))
  , Show (Push a)
  , Show (Cov (Push a))
  ) => Show (MakeSequentialState a)

newtype IvBackwardSequential m a = IvBackwardSequential
  { -- | These are all together because they need to happen transactionally, i.e. all at the same underlying Time value
    _ivBackwardSequential_subscribeUnsubscribeRead :: These (These (Cov (Push a)) (Cov (Push a))) (Cov (Pull a)) -> m ()
  }

emptyIvBackwardSequential :: Applicative m => IvBackwardSequential m a
emptyIvBackwardSequential = IvBackwardSequential
  { _ivBackwardSequential_subscribeUnsubscribeRead = \_ -> pure ()
  }

data IvForwardSequential m a = IvForwardSequential
  { _ivForwardSequential_notify :: Push a -> m ()
  , _ivForwardSequential_readResponse :: Pull a -> m ()
  }

emptyIvForwardSequential :: Applicative m => IvForwardSequential m a
emptyIvForwardSequential = IvForwardSequential
  { _ivForwardSequential_notify = \_ -> pure ()
  , _ivForwardSequential_readResponse = \_ -> pure ()
  }

