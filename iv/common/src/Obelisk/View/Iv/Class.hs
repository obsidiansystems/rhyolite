module Obelisk.View.Iv.Class where

import Prelude hiding (id, (.))

import Control.Category
import Obelisk.View.Interface
import Obelisk.View.Coverable
import Obelisk.View.CoverageMap
import Obelisk.View.Time
import Obelisk.View.Coverage
import Data.Constraint.Forall
import Control.Monad

--------------------------------------------------------------------------------
-- IvBackward
--------------------------------------------------------------------------------

-- | What you can do to the output side of an incremental view computation
--
-- A note on naming: This is a dictionary of functions. They are named with
-- verbs. They are named from the perspective of the consumer of the dictionary
-- performing these actions. E.g. the *receiver* of 'IvBackward' (from the
-- upstream IV) subscribes (to what the upstream IV publishes) by calling the
-- function in the '_ivBackward_subscribe' field.
data IvBackward m a = IvBackward
  { -- | State that push notifications are definitely needed for the given 'Cov
    -- (Push a)' at the given time; the subscription continues as long as an
    -- unsubscribe is not sent. To check whether a subscription is active at a
    -- given time, the receiver can check if: there's a subscribe for that time
    -- OR the most recent subscribe/unsubscribe prior to that time is a
    -- subscribe and all subscription activity has ceased in the intervening
    -- interval.
    --
    -- Law: you will eventually receive at notifications at least covering your
    -- query from the given time forward.
    _ivBackward_subscribe :: !(Cov (Push a) -> Time -> m ())
  , -- | State that push notifications are no longer needed for the given 'Cov
    -- (Push a)' at the given time. You must (TODO Or is it should?) have been subscribed up to this
    -- point.
    _ivBackward_unsubscribe :: !(Cov (Push a) -> Time -> m ())
  , -- | Request some information about a specific time; this is a one-off
    -- question rather than ongoing subscription.
    _ivBackward_read :: !(Cov (Pull a) -> Time -> m ())
  , -- | There won't be (and haven't been) any reads for the given times
    _ivBackward_readNone :: !(CoverageMap (WithFullCoverage (Cov (Pull a))) -> m ())
  , -- | There won't be (and haven't been) any subscribes or unsubscribes for the time intervals.
    -- If we are (or become!) subscribed or unsubscribed at the boundary of this
    -- inter, that status cascades forward.
    _ivBackward_subscribeNone :: !(CoverageMap (WithFullCoverage (Cov (Push a))) -> m ())
  }

instance Applicative m => Semigroup (IvBackward m a) where
  a <> b = IvBackward
    { _ivBackward_subscribe = \q t -> _ivBackward_subscribe a q t *> _ivBackward_subscribe b q t
    , _ivBackward_unsubscribe = \q t -> _ivBackward_unsubscribe a q t *> _ivBackward_unsubscribe b q t
    , _ivBackward_read = \q t -> _ivBackward_read a q t *> _ivBackward_read b q t
    , _ivBackward_readNone = \q -> _ivBackward_readNone a q *> _ivBackward_readNone b q
    , _ivBackward_subscribeNone = \q -> _ivBackward_subscribeNone a q *> _ivBackward_subscribeNone b q
    }

instance Applicative m => Monoid (IvBackward m a) where
  mempty = IvBackward
    { _ivBackward_subscribe = \_ _ -> pure ()
    , _ivBackward_unsubscribe = \_ _ -> pure ()
    , _ivBackward_read = \_ _ -> pure ()
    , _ivBackward_readNone = \_ -> pure ()
    , _ivBackward_subscribeNone = \_ -> pure ()
    }

liftIvBackward :: (m () -> n ()) -> IvBackward m a -> IvBackward n a
liftIvBackward f m = IvBackward
  { _ivBackward_subscribe = \n t -> f $ _ivBackward_subscribe m n t
  , _ivBackward_unsubscribe = \n t -> f $ _ivBackward_unsubscribe m n t
  , _ivBackward_subscribeNone = \q -> f $ _ivBackward_subscribeNone m q
  , _ivBackward_read = \r t -> f $ _ivBackward_read m r t
  , _ivBackward_readNone = \q -> f $ _ivBackward_readNone m q
  }


--------------------------------------------------------------------------------
-- IvForward
--------------------------------------------------------------------------------

-- | What you can do to the input side of an incremental view computation
-- Law: Two notifications or antinotifications concerning the same Time will never have overlapping queries
-- For more detail, see docs/IvInterface.md
data IvForward m a = IvForward
  { -- There was a change at the given time
    -- _ivForward_notify p n should never be called twice such that (p `overlaps` p' && t == t')
    _ivForward_notify :: !(Push a -> Time -> m ()) --TODO: Handle streaming notifications (i.e. partial transactions)
  -- There won't be (and haven't been) any notifications for the given times
  -- The combination of the coverage of all notify and notifyNone calls for a given time
  -- must add up to the portion of the keyspace that is being subscribed to.
  , _ivForward_notifyNone :: !(CoverageMap (WithFullCoverage (Cov (Push a))) -> m ())
  -- Will only be invoked at most once for every potential hole in `Pull` for a given time - i.e. (Pull, Time) pairs will never overlap
  -- TODO rename to `respondToRead`
  , _ivForward_readResponse :: !(Pull a -> Time -> m ()) --TODO: Handle streaming reads
  }

instance Applicative m => Semigroup (IvForward m a) where
  a <> b = IvForward
    { _ivForward_notify = \n t -> _ivForward_notify a n t *> _ivForward_notify b n t
    , _ivForward_readResponse = \r t -> _ivForward_readResponse a r t *> _ivForward_readResponse b r t
    , _ivForward_notifyNone = \q -> _ivForward_notifyNone a q *> _ivForward_notifyNone b q
    }

instance Applicative m => Monoid (IvForward m a) where
  mempty = IvForward
    { _ivForward_notify = \_ _ -> pure ()
    , _ivForward_readResponse = \_ _ -> pure ()
    , _ivForward_notifyNone = \_ -> pure ()
    }

liftIvForward :: (m () -> n ()) -> IvForward m a -> IvForward n a
liftIvForward f m = IvForward
  { _ivForward_notify = \n t -> f $ _ivForward_notify m n t
  , _ivForward_notifyNone = \q -> f $ _ivForward_notifyNone m q
  , _ivForward_readResponse = \r t -> f $ _ivForward_readResponse m r t
  }

--------------------------------------------------------------------------------
-- Convenience functions and types
--------------------------------------------------------------------------------

covCommutesPushPull
  :: forall i a x. (Forall (CovFunctor (Push i)), Forall (CovFunctor (Pull i)))
  => ((CovFunctor (Push i) a, CovFunctor (Pull i) a) => x)
  -> x
covCommutesPushPull x = covCommutes @(Push i) @a $ covCommutes @(Pull i) @a $ x


mapIvForward
  :: Monad m
  => (Push a' -> Push a)
  -> (CoverageMap (WithFullCoverage (Cov (Push a'))) -> CoverageMap (WithFullCoverage (Cov (Push a))))
  -> (Pull a' -> Pull a)
  -> IvForward m a -> IvForward m a'
mapIvForward mapNotify mapNotifyNone mapReadResponse u = IvForward
  { _ivForward_notify = _ivForward_notify u . mapNotify
  , _ivForward_notifyNone = _ivForward_notifyNone u . mapNotifyNone
  , _ivForward_readResponse = _ivForward_readResponse u . mapReadResponse
  }

mapIvForwardMaybe
  :: Monad m
  => (Push a' -> Maybe (Push a))
  -> (CoverageMap (WithFullCoverage (Cov (Push a'))) -> CoverageMap (WithFullCoverage (Cov (Push a))))
  -> (Pull a' -> Maybe (Pull a))
  -> IvForward m a -> IvForward m a'
mapIvForwardMaybe f g h u = IvForward
  { _ivForward_notify = \n t ->
      forM_ (f n) $ \n' -> _ivForward_notify u n' t
  , _ivForward_notifyNone = _ivForward_notifyNone u . g
  , _ivForward_readResponse = \r t ->
      forM_ (h r) $ \r' -> _ivForward_readResponse u r' t
  }

mapIvBackward
  :: Monad m
  => (Cov (Push a') -> Cov (Push a))
  -> (CoverageMap (WithFullCoverage (Cov (Push a'))) -> CoverageMap (WithFullCoverage (Cov (Push a))))
  -> (Cov (Pull a') -> Cov (Pull a))
  -> (CoverageMap (WithFullCoverage (Cov (Pull a'))) -> CoverageMap (WithFullCoverage (Cov (Pull a))))
  -> IvBackward m a -> IvBackward m a'
mapIvBackward mapSubscribe mapSubscribeNone mapRead mapReadNone u = IvBackward
  { _ivBackward_subscribe = _ivBackward_subscribe u . mapSubscribe
  , _ivBackward_unsubscribe = _ivBackward_unsubscribe u . mapSubscribe
  , _ivBackward_read = _ivBackward_read u . mapRead
  , _ivBackward_readNone = _ivBackward_readNone u . mapReadNone
  , _ivBackward_subscribeNone = _ivBackward_subscribeNone u . mapSubscribeNone
  }

mapIvBackwardMaybe
  :: Monad m
  => (Cov (Push a') -> Maybe (Cov (Push a)))
  -> (CoverageMap (WithFullCoverage (Cov (Push a'))) -> CoverageMap (WithFullCoverage (Cov (Push a))))
  -> (Cov (Pull a') -> Maybe (Cov (Pull a)))
  -> (CoverageMap (WithFullCoverage (Cov (Pull a'))) -> CoverageMap (WithFullCoverage (Cov (Pull a))))
  -> IvBackward m a -> IvBackward m a'
mapIvBackwardMaybe f h g i u = IvBackward
  { _ivBackward_subscribe = \q t ->
      forM_ (f q) $ \q' -> _ivBackward_subscribe u q' t
  , _ivBackward_unsubscribe = \q t ->
      forM_ (f q) $ \q' -> _ivBackward_unsubscribe u q' t
  , _ivBackward_read = \q t ->
      forM_ (g q) $ \q' -> _ivBackward_read u q' t
  , _ivBackward_readNone =
      _ivBackward_readNone u . i
  , _ivBackward_subscribeNone =
      _ivBackward_subscribeNone u . h
  }
