{-# LANGUAGE UndecidableInstances #-}
module Obelisk.View.DefMap where

import Prelude hiding (filter)

import Data.Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import Data.Bifunctor
import Data.Witherable
import Obelisk.View.Misc
import Test.QuickCheck
import GHC.TypeLits
import GHC.Generics
import Control.Lens (Lens', lens)

data DefMap k v = DefMap v (Map k v) deriving (Eq, Ord, Show, Generic)

instance (FromJSON v , FromJSONKey k , Ord k) => FromJSON (DefMap k v)
instance (ToJSON v , ToJSONKey k) => ToJSON (DefMap k v)

-- | DefMap cannot be a Functor because it needs Eq to deduplicate the values; otherwise, you could end up with non-canonical DefMaps
instance TypeError ('Text "DefMap cannot be a Functor; use mapDefMap instead") => Functor (DefMap k) where
  fmap = error "Impossible instance"

defMap :: Eq v => v -> Map k v -> DefMap k v
defMap d m = DefMap d $ filter (/= d) m

isConstantDefMap :: DefMap k v -> Bool
isConstantDefMap (DefMap _ m) = null m

emptyDefMap :: v -> DefMap k v
emptyDefMap v = DefMap v Map.empty

singletonDefMap :: (Ord k, Eq v) => v -> k -> v -> DefMap k v
singletonDefMap d k v = insertDefMap k v $ emptyDefMap d

lookupDefMap :: Ord k => k -> DefMap k v -> v
lookupDefMap k (DefMap d m) = Map.findWithDefault d k m

atDefMap :: (Ord k, Eq v) => k -> Lens' (DefMap k v) v
atDefMap k = lens (lookupDefMap k) (\xs v -> insertDefMap k v xs)

-- TODO name unsafe
mapDefMapKeys :: Ord b => (a -> b) -> DefMap a v -> DefMap b v
mapDefMapKeys f (DefMap d m) = DefMap d $ Map.fromList $ fmap (first f) $ Map.toList m

mapDefMap :: Eq b => (a -> b) -> DefMap k a -> DefMap k b
mapDefMap f (DefMap oldD m) = DefMap newD $ mapMaybe g m
  where newD = f oldD
        g a =
          let b = f a
          in if b == newD
             then Nothing
             else Just b

insertDefMap :: (Ord k, Eq v) => k -> v -> DefMap k v -> DefMap k v
insertDefMap k v (DefMap d m) = DefMap d $
  if v == d
  then Map.delete k m
  else Map.insert k v m

zipDefMapWith :: (Ord k, Eq c) => (a -> b -> c) -> DefMap k a -> DefMap k b -> DefMap k c
zipDefMapWith f (DefMap da ma) (DefMap db mb) = DefMap dc $ Map.merge (Map.mapMaybeMissing $ \_ -> dropDefaults . (`f` db)) (Map.mapMaybeMissing $ \_ -> dropDefaults . f da) (Map.zipWithMaybeMatched $ \_ a b -> dropDefaults $ f a b) ma mb
  where dc = f da db
        dropDefaults = nothingEq dc

instance (Ord k, Semigroup v, Eq v) => Semigroup (DefMap k v) where
  (<>) = zipDefMapWith (<>)

instance (Ord k, Monoid v, Eq v) => Monoid (DefMap k v) where
  mempty = emptyDefMap mempty

instance (Arbitrary k, Ord k, Arbitrary v, Eq v) => Arbitrary (DefMap k v) where
  arbitrary = defMap <$> arbitrary <*> arbitrary
