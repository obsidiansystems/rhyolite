{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Obelisk.View.Fan
  ( FanPushState
  , FanPullState
  , fanPull
  , fanPush
  ) where

import Prelude hiding (read)

import Control.Lens
import Control.Monad
import Control.Monad.Ref.Restricted
import Data.Bifunctor
import Data.Foldable
import Data.IntMap.Strict (IntMap)
import Data.Map.Strict (Map)
import Data.Maybe hiding (mapMaybe)
import Data.These
import Data.Tuple (swap)
import Data.Witherable
import GHC.Stack
import Obelisk.Reflex.Orphans ()
import Obelisk.View.Collidable
import Obelisk.View.Coverable
import Obelisk.View.Coverage
import Obelisk.View.CoverageMap
import Obelisk.View.DefMap
-- import Obelisk.View.Iv.Class
import Obelisk.View.Misc
import Obelisk.View.NonEmptyInterval
import Obelisk.View.OccurrenceMap
import Obelisk.View.SubscriptionMap
import Obelisk.View.SubscriptionMap.Internal.Class (fulfillSubscriptionMapFull)
import Obelisk.View.SubscriptionMap.Internal.Faster
import Obelisk.View.Time
import qualified Control.Monad.State.Strict as State
import qualified Control.Monad.Writer.CPS as CPS
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Obelisk.View.IvMonad


data FanPushState k a = FanPushState
  { _fanPushState_input :: SubscriptionMap (Cov (Map k a))
  , _fanPushState_output :: SubscriptionMap (Cov a)
  , _fanPushState_pendingNotify :: Maybe (OccurrenceMap a)
  , _fanPushState_pendingNotifyNone :: CoverageMap (WithFullCoverage (Cov a))
  }

deriving instance
  ( Show k
  , Show (Cov a)
  , Show (WithFullCoverage (Cov a))
  , Show a
  ) => Show (FanPushState k a)

makeLenses ''FanPushState


--------------------------------------------------------------------------------
-- Pull handling
--------------------------------------------------------------------------------

data FanPullState k a = FanPullState
  { _fanPullState_waitingReads :: !(IntMap (Map k (Cov a)))
  , _fanPullState_readData :: !(IntMap a)
  , _fanPullState_doneReading :: !(CoverageMap (WithFullCoverage (Map k (Cov a))))
  }

deriving instance (Show k, Show a, Show (Cov a), Show (WithFullCoverage (Cov a))) => Show (FanPullState k a)

fanPull
  :: forall m k a
  .  ( IvMonad m
     , Ord k
     , Coverable a
     , Collidable a
     , Show (Collision a)
     , Coverage (Cov a)
     , RefData m (FanPullState k a)
     , HasCallStack
     )
  => (Cov a -> Time -> m ()) -- Pull request to input side
  -> (CoverageMap (WithFullCoverage (Cov a)) -> m ()) -- readNone to input side
  -> (Map k a -> Time -> m ()) -- Pull response to output side
  -> m ( Cov (Map k a) -> Time -> m () -- Pull request from output side
       , CoverageMap (WithFullCoverage (Cov (Map k a))) -> m () -- readNone from output side
       , a -> Time -> m () -- Pull response from input side
       )
fanPull read readNone readResponse = do
  myState <- newRef $ FanPullState
    { _fanPullState_waitingReads = mempty
    , _fanPullState_readData = mempty
    , _fanPullState_doneReading = emptyCoverageMap
    }
  pure ( \q t -> join $ atomicModifyRef' myState $ \old -> -- read
           let alreadyRequested = unionMaybeCoverage
                 (Map.foldl' unionMaybeCoverage Nothing $ fmap Just $ fromMaybe mempty $ IntMap.lookup t $ _fanPullState_waitingReads old)
                 (fmap covered $ IntMap.lookup t $ _fanPullState_readData old)
               requestedCoverage = Map.foldl' unionMaybeCoverage Nothing $ fmap Just q
               newCoverage = requestedCoverage `differenceMaybeCoverage` alreadyRequested
               toSendImmediately = do
                 d <- IntMap.lookup t $ _fanPullState_readData old
                 let m = Map.mapMaybe (\q' -> restrictCoverage q' d) q
                 if Map.null m then Nothing else Just m
           in ( old
                { _fanPullState_waitingReads = IntMap.alter (Just . (\m -> Map.differenceWith (\c a -> c `differenceCoverage` covered a) m $ fromMaybe Map.empty toSendImmediately) . Map.unionWith unionCoverage q . fromMaybe mempty) t $ _fanPullState_waitingReads old --TODO: Assert that the unionCoverage has no overlap
                , _fanPullState_doneReading = _fanPullState_doneReading old `unionCoverageMaps` singletonCoverageMap t (toWithFullCoverage q)
                }
              , do forM_ newCoverage $ \nc -> do
                     read nc t
                   forM_ toSendImmediately $ \p -> do
                     readResponse p t
              )
       , \q -> do -- readNone
           join $ atomicModifyRef' myState $ \s ->
             let newCoverage = q
                 oldCoverage = _fanPullState_doneReading s
                 newDoneReading = unionCoverageMaps newCoverage oldCoverage
                 -- newlyDone = differenceCoverageMaps (mapMaybeCoverageMap minCoverageAcrossKeys newCoverage) (mapMaybeCoverageMap minCoverageAcrossKeys oldCoverage)
                 newlyDone = differenceCoverageMaps (mapMaybeCoverageMap minCoverageAcrossKeys newDoneReading) (mapMaybeCoverageMap minCoverageAcrossKeys oldCoverage)
                 toReadNone = differenceCoverageMaps newlyDone $ unionCoverageMaps
                    (intMapToCoverageMap $ mapMaybe (foldl unionMaybeCoverage Nothing . fmap (Just . toWithFullCoverage)) $ _fanPullState_waitingReads s)
                    (intMapToCoverageMap $ fmap (toWithFullCoverage . covered) $ _fanPullState_readData s)
             in ( s
                  { _fanPullState_doneReading = newDoneReading
                  , _fanPullState_readData = restrictWithFullCoverageMap (negateCoverageMap newlyDone) $ _fanPullState_readData s
                  }
                , unless (nullCoverageMap  toReadNone) (readNone toReadNone)
                )
       , \v t -> do -- readResponse
           reqs <- atomicModifyRef' myState $ \s ->
             ( s --TODO: Clear out requests we're presently responding to
               { _fanPullState_readData = case xorMaybeCoverage (Just fullCoverage) $ minCoverageAcrossKeys =<< lookupCoverageMap t (_fanPullState_doneReading s) of
                   Nothing -> _fanPullState_readData s
                   Just (coverageToKeep :: WithFullCoverage (Cov a)) -> case restrictWithFullCoverage coverageToKeep v of
                     Nothing -> _fanPullState_readData s
                     Just (dataToKeep :: a) -> IntMap.insertWith mergeAssertDisjoint t dataToKeep $ _fanPullState_readData s
               , _fanPullState_waitingReads = IntMap.alter (nothingIfNull . mapMaybe (\oldWaiting -> oldWaiting `differenceCoverage` covered v) . fromMaybe Map.empty) t $ _fanPullState_waitingReads s
               }
             , _fanPullState_waitingReads s
             )
           let availableCoverage = covered v
               v' = flip Map.mapMaybe (fromMaybe mempty $ IntMap.lookup t reqs) $ \q -> --TODO: Don't iterate
                 case q `intersectionCoverage` availableCoverage of
                   Nothing -> Nothing
                   Just _ -> restrictCoverage q v
           when (not $ Map.null v') $ do
             readResponse v' t
       )

minCoverageAcrossKeys :: Coverage a => DefMap k (Maybe a) -> Maybe a
minCoverageAcrossKeys (DefMap d m) = foldl' intersectionMaybeCoverage d m

--------------------------------------------------------------------------------
-- Push handling
--------------------------------------------------------------------------------

fanPush
  :: forall m k a
  .  ( IvMonad m
     , Ord k
     , Show k
     , Coverable a
     , Coverage (Cov a)
     , Collidable a
     , Show (Collision a)
     , HasCallStack
     , Show (WithFullCoverage (Cov a))
     , RefData m (FanPushState k a)
     , Show (Cov a)
     )
  => (Cov a -> Time -> m ()) -- Subscribe to input side
  -> (Cov a -> Time -> m ()) -- Unsubscribe to input side
  -> (CoverageMap (WithFullCoverage (Cov a)) -> m ())
  -> (Map k a -> Time -> m ()) -- Notify to output side
  -> (CoverageMap (WithFullCoverage (Cov (Map k a))) -> m ()) -- notifyNone to output side
  -> m ( Cov (Map k a) -> Time -> m () -- Subscribe from output side
       , Cov (Map k a) -> Time -> m () -- Unsubscribe from output side
       , CoverageMap (WithFullCoverage (Cov (Map k a))) -> m () -- subscribeNone from output side
       , a -> Time -> m () -- Notify from input side
       , CoverageMap (WithFullCoverage (Cov a)) -> m () -- notifyNone from input side
       )
fanPush subscribe unsubscribe subscribeNone notify notifyNone = do
  myState :: Ref m (FanPushState k a) <- newRef $ FanPushState emptySubscriptionMap emptySubscriptionMap Nothing emptyCoverageMap
  let

    collectUnusedNotifications :: FanPushState k a -> FanPushState k a
    collectUnusedNotifications oldState =
      let
        uncollectable :: CoverageMap (WithFullCoverage (Cov a))
        uncollectable = differenceCoverageMaps fullCoverage collectable
        collectable = mapCoverageMapMaybe (\(DefMap d x) -> foldl intersectionMaybeCoverage d x) $ knownNonsubscriptions $ _fanPushState_input oldState
        collected = intersectionCoverageMaps collectable $
          maybe emptyCoverageMap (toWithFullCoverage . covered) (_fanPushState_pendingNotify oldState)
          `unionCoverage`
          _fanPushState_pendingNotifyNone oldState

      in oldState
        & over fanPushState_pendingNotify (restrictMaybeWithFullCoverage $ Just uncollectable)
        . over fanPushState_pendingNotifyNone (intersectionCoverageMaps uncollectable)
        . over fanPushState_output (fulfillSubscriptionMapFull collected)

    canonicalize :: (State.State (FanPushState k a) ()) -> m ()
    canonicalize go = join $ atomicModifyRef' myState $ \oldState -> swap . flip State.runState oldState $ do

      _ <- go

      newState <- State.get
      let

        -- we need to become subscribed to the following
        newSubscriptionsCov = mapCoverageMapMaybe unionCoverages (knownSubscriptions $ _fanPushState_input newState)
          `differenceCoverageMaps` knownSubscriptions (_fanPushState_output newState)
          `differenceWithFullCoverageMaps` knownNonsubscriptions (_fanPushState_output newState)

        -- we should become unsubscribed to the following
        newUnsubscriptionsCov :: CoverageMap (WithFullCoverage (Cov a))
        newUnsubscriptionsCov = mapCoverageMapMaybe (\(DefMap d k) -> foldr intersectionMaybeCoverage d k) (fullBaseCoverageMapToFullCoverageMap $ _subscriptionMap_unsubscribed $ _fanPushState_input newState)
          `differenceCoverageMaps` toWithFullCoverage (knownSubscriptions $ _fanPushState_output newState)
          `differenceCoverageMaps` knownNonsubscriptions (_fanPushState_output newState)

        newSubscriptions, newUnsubscriptions :: Maybe (OccurrenceMap (Cov a))
        newSubscriptionsSubNone :: CoverageMap (Cov a)
        (newSubscriptions, newSubscriptionsSubNone) = first occurrenceMap $ partitionBottomEdge newSubscriptionsCov

        formerlySubscribedUnsubs = ifoldr
          (\t x -> unionMaybeCoverage $ fmap (singletonOccurrenceMap $ succ t) $ intersectionWithFullMaybeCoverage (Just x) (lookupCoverageMap (succ t) newUnsubscriptionsCov) )
          Nothing
          (_subscriptionMap_defaultSubscribe $ _fanPushState_output newState)

        floatingUnsubs = flip intersectionWithFullMaybeCoverage (Just newUnsubscriptionsCov)
          $ occurrenceMap $ IntMap.mapMaybe unionCoverages $ bottomEdge $ _fullBaseCoverageMap_floating $ _subscriptionMap_unsubscribed $ _fanPushState_input newState

        newUnsubscriptions = formerlySubscribedUnsubs `unionMaybeCoverage` floatingUnsubs

        newSubscribeNones :: CoverageMap (WithFullCoverage (Cov a))
        newSubscribeNones = toWithFullCoverage newSubscriptionsSubNone
          `unionCoverage` (differenceCoverageMaps newUnsubscriptionsCov $ maybe emptyCoverageMap toWithFullCoverage newUnsubscriptions)

        fulfulledNotifies :: Maybe (OccurrenceMap (Map k a))
        fulfulledNotifies =
          let pendingNotifies = foldMap unOccurrenceMap $ _fanPushState_pendingNotify newState
          in occurrenceMap $ CPS.execWriter $ flip traverseWithInterval_CoverageMap (knownSubscriptions $ _fanPushState_input newState) $ \ts xs ->
              CPS.tell $ IntMap.filter (not . Map.null) $ fmap (\x -> Map.mapMaybe (`restrictCoverage` x) xs) $ lookupIntMap ts pendingNotifies

        fulfulledNotifyNones :: CoverageMap (WithFullCoverage (Cov (Map k a)))
        fulfulledNotifyNones =
          flip mapCoverageMapMaybe (alignCoverageMap (_fanPushState_pendingNotifyNone newState) (knownSubscriptions $ _fanPushState_input newState)) $ \case
            This _ -> Nothing
            That _ -> Nothing
            These x y ->
              let
                y' = Map.mapMaybe (\y'' -> intersectionCoverage x $ toWithFullCoverage y'') y
              in if Map.null y' then Nothing else Just $ defMap Nothing $ Just <$> y'

      State.modify
        $ maybe id (over fanPushState_output . subscribeSubscriptionMapOccurenceMap) newSubscriptions
        . maybe id (over fanPushState_output . unsubscribeSubscriptionMapOccurenceMap) newUnsubscriptions
        . over fanPushState_output (checkpointSubscriptions newSubscribeNones)
        . maybe id (over fanPushState_input . fulfillSubscriptionMap . occurenceToCoverageMap . covered) fulfulledNotifies
        . over fanPushState_input (fulfillSubscriptionMapFull fulfulledNotifyNones)

      State.modify collectUnusedNotifications

      pure $ do
        for_ newSubscriptions $ traverseOccurenceMap_ subscribe
        for_ newUnsubscriptions $ traverseOccurenceMap_ unsubscribe
        unless (nullCoverageMap newSubscribeNones) $ subscribeNone newSubscribeNones
        for_ fulfulledNotifies $ traverseOccurenceMap_ notify
        unless (nullCoverageMap fulfulledNotifyNones) $ notifyNone fulfulledNotifyNones

  pure ( -- subscribe
         \(q :: Map k (Cov a)) (t :: Time) -> canonicalize $ do
          fanPushState_input %= fst . subscribeSubscriptionMap t q

       , -- unsubscribe
         \(q :: Map k (Cov a)) (t :: Time) -> canonicalize $ do
          fanPushState_input %= fst . unsubscribeSubscriptionMap t q
       , -- subscribeNone
         \cm -> canonicalize $ do
          fanPushState_input %= checkpointSubscriptions cm
       , -- notify
         \n t -> canonicalize $ do
          fanPushState_pendingNotify %= mergeAssertDisjoint (Just $ singletonOccurrenceMap t n)
       , -- notifyNone
         \cm -> canonicalize $ do
          fanPushState_pendingNotifyNone %= unionCoverage cm
       )

