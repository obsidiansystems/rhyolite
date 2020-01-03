-- | Definition, utilities and instances for 'SemiMap' and 'SemiSet'.

{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- TODO this is necessary because of the lack of a Foldable instance for Alt

module Rhyolite.SemiMap where

import Data.Aeson
import Data.Coerce
import Data.Either
import Data.Map.Monoidal as Map
import Data.Maybe
import Data.Monoid hiding (First (..), (<>))
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)

import Rhyolite.Aeson.Orphans ()

-- | A SemiMap is a structure built on top on the 'MonoidalMap' that lets you
-- distinguish two semantic meanings of the monoidal map:
data SemiMap k v
   = SemiMap_Complete (MonoidalMap k v)
   -- ^ The MonoidalMap contains all the information that I want to express.
   | SemiMap_Partial (MonoidalMap k (First (Maybe v)))
   -- ^ I'm only expressing a patch for some information that I already possess.
   deriving (Show, Read, Eq, Ord, Foldable, Functor, Generic)

isComplete :: SemiMap k v -> Bool
isComplete = isJust . getComplete

getComplete :: SemiMap k v -> Maybe (MonoidalMap k v)
getComplete = \case
  SemiMap_Complete m -> Just m
  SemiMap_Partial _ -> Nothing

knownKeysSet :: SemiMap k v -> Set k
knownKeysSet = \case
  SemiMap_Complete x -> Map.keysSet x
  SemiMap_Partial x -> Map.keysSet $ Map.mapMaybe getFirst x

knownKeys :: SemiMap k v -> [k]
knownKeys = Set.toList . knownKeysSet

knownSubMap :: SemiMap k v -> MonoidalMap k v
knownSubMap = \case
  SemiMap_Complete m -> m
  SemiMap_Partial m -> Map.mapMaybe getFirst m

instance (Ord k) => Monoid (SemiMap k v) where
  mempty = SemiMap_Partial mempty

instance (Ord k) => Semigroup (SemiMap k v) where
  new <> old = case new of
    SemiMap_Complete _ -> new
    SemiMap_Partial p -> case old of
      SemiMap_Partial oldp -> SemiMap_Partial $ p <> oldp
      SemiMap_Complete oldc -> SemiMap_Complete $ applyMap (coerce p) (coerce oldc)
      where
        applyMap :: Ord k => MonoidalMap k (Maybe v) -> MonoidalMap k v -> MonoidalMap k v
        applyMap patch old' = Map.unionWith const insertions (old' `Map.difference` deletions)
          where (deletions, insertions) = mapPartitionEithers $ maybeToEither <$> patch
                maybeToEither = \case
                  Nothing -> Left ()
                  Just r -> Right r
        mapPartitionEithers :: MonoidalMap k (Either a b) -> (MonoidalMap k a, MonoidalMap k b)
        mapPartitionEithers m = (unsafeFromLeft <$> ls, unsafeFromRight <$> rs)
          where (ls, rs) = Map.partition isLeft m
                unsafeFromLeft (Left l) = l
                unsafeFromLeft _ = error "mapPartitionEithers: fromLeft received a Right value; this should be impossible"
                unsafeFromRight (Right r) = r
                unsafeFromRight _ = error "mapPartitionEithers: fromRight received a Left value; this should be impossible"

instance (ToJSON k, ToJSON v, ToJSONKey k) => ToJSON (SemiMap k v)
instance (Ord k, FromJSON k, FromJSON v, FromJSONKey k) => FromJSON (SemiMap k v)

-- | With a SemiSet, you can express that you have a complete set of elements,
-- or a patch that modifies another collection.
type SemiSet k = SemiMap k ()

fromKnown :: Set k -> SemiSet k
fromKnown = SemiMap_Partial . Map.fromSet (\_ -> First $ Just ())

fromKnownAbsent :: Set k -> SemiMap k ()
fromKnownAbsent = SemiMap_Partial . Map.fromSet (\_ -> First Nothing)

fromKnownComplete :: Set k -> SemiMap k ()
fromKnownComplete = SemiMap_Complete . Map.fromSet (\_ -> ())

-- | A SemiMap that knows whether a particular item is present, but doesn't know
-- anything about the presence or absence of other items.
singleKnownPresence
  :: a -- ^ The item
  -> Bool -- ^ Whether it is present
  -> SemiSet a
singleKnownPresence a b = f $ Set.singleton a
  where f = case b of
          False -> fromKnownAbsent
          True -> fromKnown
