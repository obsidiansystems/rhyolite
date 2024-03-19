-- | Definition, utilities and instances for 'SemiMap' and 'SemiSet'.

{-# Language DeriveFoldable #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language LambdaCase #-}

module Rhyolite.SemiMap where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Align
import Data.Coerce (coerce)
import Data.Map.Monoidal as Map
import Data.Maybe (isJust)
import Data.Semigroup (First(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.These
import GHC.Generics (Generic)

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

null :: SemiMap k v -> Bool
null = \case
  SemiMap_Partial m -> Map.null m
  SemiMap_Complete _ -> False

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
          where (deletions, insertions) = Map.mapEither maybeToEither patch
                maybeToEither = \case
                  Nothing -> Left ()
                  Just r -> Right r

-- | Given a new and an old Map, create a Partial SemiMap which would produce
-- the new one given the old one
semiMapDiff
  :: ( Ord k
     , Eq v
     )
  => MonoidalMap k v -- ^ New
  -> MonoidalMap k v -- ^ Old
  -> SemiMap k v
semiMapDiff new old = SemiMap_Partial $ Map.mapMaybe id $ alignWith f new old
  where f = \case
          This newV -> Just $ First $ Just newV
          That _ -> Just $ First Nothing
          These newV oldV
            | newV == oldV -> Nothing
            | otherwise -> Just $ First $ Just newV

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
