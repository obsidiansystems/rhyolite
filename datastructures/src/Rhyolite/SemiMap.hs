{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- TODO this is necessary because of the lack of a Foldable instance for Alt

module Rhyolite.SemiMap where

import Data.Aeson
import Data.Coerce
import Data.Either
import Data.Maybe
import Data.Monoid hiding ((<>), First (..))
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.AppendMap (AppendMap (..))
import qualified Data.AppendMap as Map
import Rhyolite.Aeson.Orphans ()
import GHC.Generics

data SemiMap k v
   = SemiMap_Complete (AppendMap k v)
   | SemiMap_Partial (AppendMap k (First (Maybe v)))
   deriving (Show, Read, Eq, Ord, Foldable, Functor, Generic)

isComplete :: SemiMap k v -> Bool
isComplete = isJust . getComplete

getComplete :: SemiMap k v -> Maybe (AppendMap k v)
getComplete = \case
  SemiMap_Complete m -> Just m
  SemiMap_Partial _ -> Nothing

knownKeysSet :: SemiMap k v -> Set k
knownKeysSet = \case
  SemiMap_Complete x -> Map.keysSet x
  SemiMap_Partial x -> Map.keysSet $ Map.mapMaybe getFirst x

knownKeys :: SemiMap k v -> [k]
knownKeys = Set.toList . knownKeysSet

knownSubMap :: SemiMap k v -> AppendMap k v
knownSubMap = \case
  SemiMap_Complete m -> m
  SemiMap_Partial m -> Map.mapMaybe getFirst m

deriving instance Foldable f => Foldable (Alt f)

instance (Ord k) => Monoid (SemiMap k v) where
  mempty = SemiMap_Partial mempty

instance (Ord k) => Semigroup (SemiMap k v) where
  new <> old = case new of
    SemiMap_Complete _ -> new
    SemiMap_Partial p -> case old of
      SemiMap_Partial oldp -> SemiMap_Partial $ p <> oldp
      SemiMap_Complete oldc -> SemiMap_Complete $ applyMap (coerce p) (coerce oldc)
      where
        applyMap :: Ord k => AppendMap k (Maybe v) -> AppendMap k v -> AppendMap k v
        applyMap patch old' = Map.unionWith const insertions (old' `Map.difference` deletions)
          where (deletions, insertions) = mapPartitionEithers $ maybeToEither <$> patch
                maybeToEither = \case
                  Nothing -> Left ()
                  Just r -> Right r
        mapPartitionEithers :: AppendMap k (Either a b) -> (AppendMap k a, AppendMap k b)
        mapPartitionEithers m = (fromLeft <$> ls, fromRight <$> rs)
          where (ls, rs) = Map.partition isLeft m
                fromLeft (Left l) = l
                fromLeft _ = error "mapPartitionEithers: fromLeft received a Right value; this should be impossible"
                fromRight (Right r) = r
                fromRight _ = error "mapPartitionEithers: fromRight received a Left value; this should be impossible"

instance (ToJSON k, ToJSON v) => ToJSON (SemiMap k v)
instance (Ord k, FromJSON k, FromJSON v) => FromJSON (SemiMap k v)

type SemiSet k = SemiMap k ()

fromKnown :: Set k -> SemiSet k
fromKnown = SemiMap_Partial . Map.fromSet (\_ -> First $ Just ())

fromKnownAbsent :: Set k -> SemiMap k ()
fromKnownAbsent = SemiMap_Partial . Map.fromSet (\_ -> First Nothing)

fromKnownComplete :: Set k -> SemiMap k ()
fromKnownComplete = SemiMap_Complete . Map.fromSet (\_ -> ())

