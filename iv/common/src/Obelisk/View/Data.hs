{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Obelisk.View.Data where

import Control.Lens
import Data.Maybe (fromMaybe, isNothing)
import Control.Monad
import Control.Monad.Ref.Restricted
import GHC.Generics
import Obelisk.View.Collidable
import Obelisk.View.Coverable
import Obelisk.View.Coverage
import Obelisk.View.CoverageMap
import Obelisk.View.Iv.Class
import Obelisk.View.Interface
import Obelisk.View.OccurrenceMap
import Obelisk.View.Time
import Obelisk.View.IvMonad
import Obelisk.View.NonEmptyInterval
import Data.Bifunctor (first)

data IvDataForward a = IvDataForward
  { _ivDataForward_notifies :: OccurrenceMap (Push a)
  , _ivDataForward_notifyNones :: CoverageMap (WithFullCoverage (Cov (Push a)))
  , _ivDataForward_readResponses :: OccurrenceMap (Pull a)
  }

emptyIvDataForward :: IvDataForward a
emptyIvDataForward = IvDataForward
  { _ivDataForward_readResponses = unsafeEmptyOccurenceMap
  , _ivDataForward_notifies = unsafeEmptyOccurenceMap
  , _ivDataForward_notifyNones = emptyCoverageMap
  }
instance
    ( Collidable (Push a), Show (Collision (Push a))
    , Coverage (WithFullCoverage (Cov (Push a)))
    , Collidable (Pull a), Show (Collision (Pull a))
    ) => Semigroup (IvDataForward a) where
  a <> b = IvDataForward
    { _ivDataForward_notifies = _ivDataForward_notifies a `mergeAssertDisjoint` _ivDataForward_notifies b
    , _ivDataForward_notifyNones = _ivDataForward_notifyNones a `unionCoverage` _ivDataForward_notifyNones b
    , _ivDataForward_readResponses = _ivDataForward_readResponses a `mergeAssertDisjoint` _ivDataForward_readResponses b
    }

instance
    ( Collidable (Push a), Show (Collision (Push a))
    , Coverage (WithFullCoverage (Cov (Push a)))
    , Collidable (Pull a), Show (Collision (Pull a))
    ) => Monoid (IvDataForward a) where
  mempty = IvDataForward unsafeEmptyOccurenceMap emptyCoverageMap unsafeEmptyOccurenceMap

deriving instance (Show (Push a), Show (Pull a), Show (WithFullCoverage (Cov (Push a)))) => Show (IvDataForward a)
deriving instance (Eq (Push a), Eq (Pull a), Eq (WithFullCoverage (Cov (Push a)))) => Eq (IvDataForward a)

instance (Collidable (Pull a), Coverage (WithFullCoverage (Cov (Push a)))) => Semigroup (IvDataForwardCollision a) where
  a <> b = IvDataForwardCollision
    { _ivDataForwardCollision_notify = _ivDataForwardCollision_notify a `unionCoverage` _ivDataForwardCollision_notify b
    , _ivDataForwardCollision_readResponse = _ivDataForwardCollision_readResponse a <> _ivDataForwardCollision_readResponse b
    }

instance (Collidable (Pull a), Coverage (WithFullCoverage (Cov (Push a)))) => Monoid (IvDataForwardCollision a) where
  mempty = IvDataForwardCollision
    { _ivDataForwardCollision_notify = emptyCoverageMap
    , _ivDataForwardCollision_readResponse = mempty
    }
  mappend = (<>)

instance
  ( Collidable (Push a)
  , Collidable (Pull a)
  , Coverable (Push a)
  , Coverage (Cov (Push a))
  , Coverage (Cov (Pull a))
  , Show (Collision (Push a))
  ) => Collidable (IvDataForward a) where
  type Collision (IvDataForward a) = IvDataForwardCollision a
  mergeDisjoint a b =
    let (notifyCollision, _) = unionWithOverlap
          [ _ivDataForward_notifyNones a
          , _ivDataForward_notifyNones b
          , toWithFullCoverage . covered $ _ivDataForward_notifies a
          , toWithFullCoverage . covered $ _ivDataForward_notifies b
          ]
        readResponses = mergeDisjoint (_ivDataForward_readResponses a) (_ivDataForward_readResponses b)
    in case (notifyCollision, readResponses) of
         (Nothing, Right r) -> Right $ IvDataForward
           { _ivDataForward_notifies = mergeAssertDisjoint (_ivDataForward_notifies a) (_ivDataForward_notifies b)
           , _ivDataForward_notifyNones = unionCoverageMaps (_ivDataForward_notifyNones a) (_ivDataForward_notifyNones b)
           , _ivDataForward_readResponses = r
           }
         _ -> Left $ IvDataForwardCollision
           { _ivDataForwardCollision_notify = fromMaybe emptyCoverageMap notifyCollision
           , _ivDataForwardCollision_readResponse = either id (const mempty) readResponses
           }

data IvDataForwardCollision a = IvDataForwardCollision
  { _ivDataForwardCollision_notify :: CoverageMap (WithFullCoverage (Cov (Push a)))
  , _ivDataForwardCollision_readResponse :: Collision (OccurrenceMap (Pull a))
  }
  deriving (Generic)

deriving instance
  ( Eq (Collision (Pull a))
  , Eq (WithFullCoverage (Cov (Push a)))
  ) => Eq (IvDataForwardCollision a)
deriving instance
  ( Show (Collision (Pull a))
  , Show (WithFullCoverage (Cov (Push a)))
  ) => Show (IvDataForwardCollision a)

runIvDataForwards :: Applicative m => IvDataForward i -> IvForward m i -> m ()
runIvDataForwards d f
  =  (ifor_ (_ivDataForward_notifies d) $ \t s -> _ivForward_notify f s t)
  *> (unless (nullCoverageMap $ _ivDataForward_notifyNones d) $ _ivForward_notifyNone f $ _ivDataForward_notifyNones d)
  *> (ifor_ (_ivDataForward_readResponses d) $ \t s -> _ivForward_readResponse f s t)

newtype BootForwardsState m i = BootForwardsState { unBootForwardsState :: Either (IvDataForward i) (IvForward m i) }

instance
    ( Show (Push i), Show (WithFullCoverage (Cov (Push i)))
    , Show (Pull i)
    )
    => Show (BootForwardsState m i) where
  showsPrec n (BootForwardsState x) = showParen (n >= 11)
    $ showString "BootForwardsState "
    . showParen True (case x of
      Left l -> showString "Left " . showsPrec 11 l
      Right _ -> showString "Right (IvForward {...})")

mkBootForwards :: forall m i.
  ( IvMonad m , Coverage (Cov (Push i)) , Coverage (Cov (Pull i))
  , Collidable (Push i)
  , Collidable (Pull i)
  , RefData m (BootForwardsState m i)
  , Show (Collision (Push i))
  , Show (Collision (Pull i))
  )
  => m (IvForward m i, (IvForward m i -> m ()))
mkBootForwards = do
  queued :: Ref m (BootForwardsState m i) <- newRef $ BootForwardsState $ Left mempty
  let mref f = join $ atomicModifyRef' queued $ \(BootForwardsState x) -> first BootForwardsState (f x)
  pure $ (,)
    (IvForward
      { _ivForward_notify = \s t -> mref $ \case
        Left x -> (Left (x <> mempty {_ivDataForward_notifies = singletonOccurrenceMap t s}), pure ())
        Right b -> (Right b, _ivForward_notify b s t)
      , _ivForward_notifyNone = \c -> mref $ \case
        Left x -> (Left (x <> mempty {_ivDataForward_notifyNones = c}), pure ())
        Right b -> (Right b, _ivForward_notifyNone b c)
      , _ivForward_readResponse = \s t -> mref $ \case
        Left x -> (Left (x <> mempty {_ivDataForward_readResponses = singletonOccurrenceMap t s}), pure ())
        Right b -> (Right b, _ivForward_readResponse b s t)
      })
    (\b -> mref $ \case
      Left x -> (Right b, runIvDataForwards x b)
      Right _ -> error "mkBootForwards already booted"
    )


data IvDataBackward a = IvDataBackward
  { _ivDataBackward_subscribes :: OccurrenceMap (Cov (Push a))
  , _ivDataBackward_unsubscribes :: OccurrenceMap (Cov (Push a))
  , _ivDataBackward_subscribeNones :: CoverageMap (WithFullCoverage (Cov (Push a)))
  , _ivDataBackward_reads :: OccurrenceMap (Cov (Pull a))
  , _ivDataBackward_readNones :: CoverageMap (WithFullCoverage (Cov (Pull a)))
  }

emptyIvDataBackward :: IvDataBackward a
emptyIvDataBackward = IvDataBackward
  { _ivDataBackward_reads = unsafeEmptyOccurenceMap
  , _ivDataBackward_subscribes = unsafeEmptyOccurenceMap
  , _ivDataBackward_unsubscribes = unsafeEmptyOccurenceMap
  , _ivDataBackward_subscribeNones = emptyCoverageMap
  , _ivDataBackward_readNones = emptyCoverageMap
  }


-- | Unions them all; overlaps are ignored
instance ( Coverage (Cov (Push a))
         , Coverage (Cov (Pull a))
         ) => Semigroup (IvDataBackward a) where
  a <> b = IvDataBackward
    { _ivDataBackward_subscribes = unionCoverage (_ivDataBackward_subscribes a) (_ivDataBackward_subscribes b)
    , _ivDataBackward_unsubscribes = unionCoverage (_ivDataBackward_unsubscribes a) (_ivDataBackward_unsubscribes b)
    , _ivDataBackward_subscribeNones = _ivDataBackward_subscribeNones a `unionCoverageMaps` _ivDataBackward_subscribeNones b
    , _ivDataBackward_reads = unionCoverage (_ivDataBackward_reads a) (_ivDataBackward_reads b)
    , _ivDataBackward_readNones = _ivDataBackward_readNones a `unionCoverageMaps` _ivDataBackward_readNones b
    }

instance ( Coverage (Cov (Push a))
         , Coverage (Cov (Pull a))
         ) => Monoid (IvDataBackward a) where
  mempty = IvDataBackward
    { _ivDataBackward_subscribes = unsafeEmptyOccurenceMap
    , _ivDataBackward_unsubscribes = unsafeEmptyOccurenceMap
    , _ivDataBackward_subscribeNones = emptyCoverageMap
    , _ivDataBackward_reads = unsafeEmptyOccurenceMap
    , _ivDataBackward_readNones = emptyCoverageMap
    }

deriving instance
  ( Show (Cov (Push a))
  , Show (WithFullCoverage (Cov (Push a)))
  , Show (Cov (Pull a))
  , Show (WithFullCoverage (Cov (Pull a)))
  ) => Show (IvDataBackward a)

deriving instance
  ( Eq (Cov (Push a))
  , Eq (WithFullCoverage (Cov (Push a)))
  , Eq (Cov (Pull a))
  , Eq (WithFullCoverage (Cov (Pull a)))
  ) => Eq (IvDataBackward a)

nonEmptyIvDataBackward :: IvDataBackward i -> Maybe (IvDataBackward i)
nonEmptyIvDataBackward xs@(IvDataBackward s us sn r rn) =
  if nullOccurenceMap s && nullOccurenceMap us && nullCoverageMap sn && nullOccurenceMap r && nullCoverageMap rn
  then Nothing else Just xs

splitIvDataBackwards
  :: (Coverage (Cov (Push i)), Coverage (Cov (Pull i)))
  => Time -> IvDataBackward i -> (Maybe (IvDataBackward i), Maybe (IvDataBackward i))
splitIvDataBackwards t0 d =
  let
    t :: forall x. FullCoverage x => CoverageMap x
    t = singletonRangeCoverageMap (NonEmptyInterval minBound t0) fullCoverage

    splitCoverageMapBelow, splitCoverageMapAbove :: forall x. Coverage x => CoverageMap x -> CoverageMap x 
    splitCoverageMapBelow c = fromMaybe emptyCoverageMap $ intersectionWithFullCoverage c t
    splitCoverageMapAbove c = fromMaybe emptyCoverageMap $ differenceWithFullCoverage c t

    splitOccuranceMapBelow, splitOccuranceMapAbove :: OccurrenceMap x -> OccurrenceMap x
    splitOccuranceMapBelow (OccurrenceMap c) = OccurrenceMap $ fst $ splitLEIntMap t0 c
    splitOccuranceMapAbove (OccurrenceMap c) = OccurrenceMap $ snd $ splitLEIntMap t0 c

    before = IvDataBackward
      { _ivDataBackward_subscribes = splitOccuranceMapBelow $ _ivDataBackward_subscribes d
      , _ivDataBackward_unsubscribes = splitOccuranceMapBelow $ _ivDataBackward_unsubscribes d
      , _ivDataBackward_reads = splitOccuranceMapBelow $ _ivDataBackward_reads d
      , _ivDataBackward_subscribeNones = splitCoverageMapBelow $ _ivDataBackward_subscribeNones d
      , _ivDataBackward_readNones = splitCoverageMapBelow $ _ivDataBackward_readNones d
      }
    after = IvDataBackward
      { _ivDataBackward_subscribes = splitOccuranceMapAbove $ _ivDataBackward_subscribes d
      , _ivDataBackward_unsubscribes = splitOccuranceMapAbove $ _ivDataBackward_unsubscribes d
      , _ivDataBackward_reads = splitOccuranceMapAbove $ _ivDataBackward_reads d
      , _ivDataBackward_subscribeNones = splitCoverageMapAbove $ _ivDataBackward_subscribeNones d
      , _ivDataBackward_readNones = splitCoverageMapAbove $ _ivDataBackward_readNones d
      }
  in (nonEmptyIvDataBackward before , nonEmptyIvDataBackward after)

runIvDataBackwards :: Applicative m => IvDataBackward i -> IvBackward m i -> m ()
runIvDataBackwards d b
  =  (ifor_ (_ivDataBackward_subscribes d) $ \t s -> _ivBackward_subscribe b s t)
  *> (ifor_ (_ivDataBackward_unsubscribes d) $ \t s -> _ivBackward_unsubscribe b s t)
  *> (ifor_ (_ivDataBackward_reads d) $ \t s -> _ivBackward_read b s t)
  *> (unless (nullCoverageMap $ _ivDataBackward_readNones d) $ _ivBackward_readNone b $ _ivDataBackward_readNones d)
  *> (unless (nullCoverageMap $ _ivDataBackward_subscribeNones d) $ _ivBackward_subscribeNone b $ _ivDataBackward_subscribeNones d)

newtype BootBackwardsState m i = BootBackwardsState { unBootBackwardsState :: Either (IvDataBackward i) (IvBackward m i) }

instance
    ( Show (Cov (Push i)), Show (WithFullCoverage (Cov (Push i)))
    , Show (Cov (Pull i)), Show (WithFullCoverage (Cov (Pull i)))
    )
    => Show (BootBackwardsState m i) where
  showsPrec n (BootBackwardsState x) = showParen (n >= 11)
    $ showString "BootBackwardsState "
    . showParen True (case x of
      Left l -> showString "Left " . showsPrec 11 l
      Right _ -> showString "Right (IvBackward {...})")

mkBootBackwards :: forall m i.
  ( IvMonad m, Coverage (Cov (Push i)), Coverage (Cov (Pull i)), RefData m (BootBackwardsState m i)
  )
  => m (IvBackward m i, (IvBackward m i -> m ()))
mkBootBackwards = do
  queued :: Ref m (BootBackwardsState m i) <- newRef $ BootBackwardsState $ Left mempty
  let mref f = join $ atomicModifyRef' queued $ \(BootBackwardsState x) -> first BootBackwardsState (f x)
  pure $ (,)
    (IvBackward
      { _ivBackward_subscribe = \s t -> mref $ \case
        Left x -> (Left (x <> mempty {_ivDataBackward_subscribes = singletonOccurrenceMap t s}), pure ())
        Right b -> (Right b, _ivBackward_subscribe b s t)
      , _ivBackward_unsubscribe = \s t -> mref $ \case
        Left x -> (Left (x <> mempty {_ivDataBackward_unsubscribes = singletonOccurrenceMap t s}), pure ())
        Right b -> (Right b, _ivBackward_unsubscribe b s t)
      , _ivBackward_read = \s t -> mref $ \case
        Left x -> (Left (x <> mempty {_ivDataBackward_reads = singletonOccurrenceMap t s}), pure ())
        Right b -> (Right b, _ivBackward_read b s t)
      , _ivBackward_readNone = \c -> mref $ \case
        Left x -> (Left (x <> mempty {_ivDataBackward_readNones = c}), pure ())
        Right b -> (Right b, _ivBackward_readNone b c)
      , _ivBackward_subscribeNone = \c -> mref $ \case
        Left x -> (Left (x <> mempty {_ivDataBackward_subscribeNones = c}), pure ())
        Right b -> (Right b, _ivBackward_subscribeNone b c)
      })
    (\b -> mref $ \case
      Left x -> (Right b, runIvDataBackwards x b)
      Right _ -> error "mkBootBackwards already booted"
    )

instance (Coverage (Cov (Push a)), Coverage (Cov (Pull a))) => Collidable (IvDataBackward a) where
  type Collision (IvDataBackward a) = IvDataBackwardCollision a
  mergeDisjoint a b =
    let (subscribeCollision, _) = unionWithOverlap
          [ toWithFullCoverage $ _ivDataBackward_subscribes a
          , toWithFullCoverage $ _ivDataBackward_subscribes b
          , toWithFullCoverage $ _ivDataBackward_unsubscribes a
          , toWithFullCoverage $ _ivDataBackward_unsubscribes b
          , _ivDataBackward_subscribeNones a
          , _ivDataBackward_subscribeNones b
          ]
        (readCollision, _) = unionWithOverlap
          [ toWithFullCoverage $ _ivDataBackward_reads a
          , toWithFullCoverage $ _ivDataBackward_reads b
          , _ivDataBackward_readNones a
          , _ivDataBackward_readNones b
          ]
    in if isNothing subscribeCollision && isNothing readCollision
       then Right $ IvDataBackward
            { _ivDataBackward_subscribes = _ivDataBackward_subscribes a `unionCoverage` _ivDataBackward_subscribes b
            , _ivDataBackward_unsubscribes = _ivDataBackward_unsubscribes a `unionCoverage` _ivDataBackward_unsubscribes b
            , _ivDataBackward_subscribeNones = _ivDataBackward_subscribeNones a `unionCoverage` _ivDataBackward_subscribeNones b
            , _ivDataBackward_reads = _ivDataBackward_reads a `unionCoverage` _ivDataBackward_reads b
            , _ivDataBackward_readNones = _ivDataBackward_readNones a `unionCoverage` _ivDataBackward_readNones b
            }
       else Left $ IvDataBackwardCollision
            { _ivDataBackwardCollision_subscribe = fromMaybe emptyCoverageMap subscribeCollision
            , _ivDataBackwardCollision_read = fromMaybe emptyCoverageMap readCollision
            }

data IvDataBackwardCollision a = IvDataBackwardCollision
  { _ivDataBackwardCollision_subscribe :: CoverageMap (WithFullCoverage (Cov (Push a)))
  , _ivDataBackwardCollision_read :: CoverageMap (WithFullCoverage (Cov (Pull a)))
  }
  deriving (Generic)

deriving instance
  ( Show (Cov (Push a))
  , Show (WithFullCoverage (Cov (Push a)))
  , Show (Cov (Pull a))
  , Show (WithFullCoverage (Cov (Pull a)))
  ) => Show (IvDataBackwardCollision a)

deriving instance
  ( Eq (Cov (Push a))
  , Eq (WithFullCoverage (Cov (Push a)))
  , Eq (Cov (Pull a))
  , Eq (WithFullCoverage (Cov (Pull a)))
  ) => Eq (IvDataBackwardCollision a)

instance
  ( Coverage (Cov (Push a))
  , Coverage (Cov (Pull a))
  ) => Semigroup (IvDataBackwardCollision a) where
  a <> b = IvDataBackwardCollision
    { _ivDataBackwardCollision_subscribe = _ivDataBackwardCollision_subscribe a `unionCoverage` _ivDataBackwardCollision_subscribe b
    , _ivDataBackwardCollision_read = _ivDataBackwardCollision_read a `unionCoverage` _ivDataBackwardCollision_read b
    }

instance
  ( Coverage (Cov (Push a))
  , Coverage (Cov (Pull a))
  ) => Monoid (IvDataBackwardCollision a) where
  mempty = IvDataBackwardCollision
    { _ivDataBackwardCollision_subscribe = emptyCoverageMap
    , _ivDataBackwardCollision_read = emptyCoverageMap
    }
  mappend = (<>)

-- | Returns all regions that are covered by 2 or more values
unionWithOverlap :: Coverage a => [a] -> (Maybe a, Maybe a)
unionWithOverlap [] = (Nothing, Nothing)
unionWithOverlap (h : t) = (carry `unionMaybeCoverage` (Just h `intersectionMaybeCoverage` union), Just h `unionMaybeCoverage` union)
  where (carry, union) = unionWithOverlap t

-- deriveFields ''IvDataBackward "IvDataBackwardField"
-- deriveFields ''IvDataForward "IvDataForwardField"

-- instance ArgDict c (Field (IvDataForward a)) where
--   type ConstraintsFor (Field (IvDataForward a)) c =
--     ( c (OccurrenceMap (Push a))
--     , c (CoverageMap (WithFullCoverage (Cov (Push a))))
--     , c (OccurrenceMap (Pull a))
--     )
--   argDict = \case
--     IvDataForwardField_Notifies -> Dict
--     IvDataForwardField_NotifyNones -> Dict
--     IvDataForwardField_ReadResponses -> Dict

-- instance ArgDict c (Field (IvDataBackward a)) where
--   type ConstraintsFor (Field (IvDataBackward a)) c =
--     ( c (OccurrenceMap (Cov (Push a)))
--     , c (CoverageMap (WithFullCoverage (Cov (Push a))))
--     , c (OccurrenceMap (Cov (Pull a)))
--     , c (CoverageMap (WithFullCoverage (Cov (Pull a))))
--     )
--   argDict = \case
--     IvDataBackwardField_Subscribes -> Dict
--     IvDataBackwardField_Unsubscribes -> Dict
--     IvDataBackwardField_SubscribeNones -> Dict
--     IvDataBackwardField_Reads -> Dict
--     IvDataBackwardField_ReadNones -> Dict


-- | Wrap an IvForward so that it also collects all its data into the given callback
collectIvForward :: (IvDataForward a -> m ()) -> IvForward m a
collectIvForward cb = IvForward
  { _ivForward_notify = \n t -> do
      cb $ emptyIvDataForward
        { _ivDataForward_notifies = singletonOccurrenceMap t n
        }
  , _ivForward_notifyNone = \q -> do
      cb $ emptyIvDataForward
        { _ivDataForward_notifyNones = q
        }
  , _ivForward_readResponse = \p t -> do
      cb $ emptyIvDataForward
        { _ivDataForward_readResponses = singletonOccurrenceMap t p
        }
  }

-- | Wrap an IvBackward so that it also collects all its data into the given callback
collectIvBackward :: (IvDataBackward a -> m ()) -> IvBackward m a
collectIvBackward cb = IvBackward
  { _ivBackward_subscribe = \n t -> do
      cb $ emptyIvDataBackward
        { _ivDataBackward_subscribes = singletonOccurrenceMap t n
        }
  , _ivBackward_unsubscribe = \n t -> do
      cb $ emptyIvDataBackward
        { _ivDataBackward_unsubscribes = singletonOccurrenceMap t n
        }
  , _ivBackward_subscribeNone = \q -> do
      cb $ emptyIvDataBackward
        { _ivDataBackward_subscribeNones = q
        }
  , _ivBackward_read = \p t -> do
      cb $ emptyIvDataBackward
        { _ivDataBackward_reads = singletonOccurrenceMap t p
        }
  , _ivBackward_readNone = \q -> do
      cb $ emptyIvDataBackward
        { _ivDataBackward_readNones = q
        }
  }

emptyIvForward :: Applicative m => IvForward m a
emptyIvForward = mempty


emptyIvBackward :: Applicative m => IvBackward m a
emptyIvBackward = mempty

