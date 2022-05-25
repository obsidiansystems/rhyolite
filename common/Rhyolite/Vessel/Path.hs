{-# Language FlexibleInstances #-}
{-# Language FunctionalDependencies #-}
{-# Language RankNTypes #-}
{-# Language GADTs #-}
module Rhyolite.Vessel.Path
  (module Rhyolite.Vessel.Path, module Data.Vessel.Path)
  where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Monoidal
import Data.Semigroup
import Data.Vessel
import Data.Vessel.Path
import Rhyolite.SemiMap (SemiMap, getComplete)

-- | 'Path' that will process the result and extract complete SemiMaps.
semiMapP :: (Traversable f) => Path x x (f (SemiMap k v)) (f (Map k v))
semiMapP = postMap (traverse (fmap getMonoidalMap . getComplete))

-- | 'Path' that will process the result and collapse a Map of SemiMaps to extract just those
-- which are complete.
semiMapsP :: (Traversable f) => Path x x (f (Map k (SemiMap k' v))) (f (Map k (Map k' v)))
semiMapsP = postMap (traverse (Just . Map.mapMaybe (fmap getMonoidalMap . getComplete)))

-- TODO: Move to Data.Vessel.Path
-- | 'Path' that will process the result and extract a 'First'.
firstP :: Traversable f => Path x x (f (First v)) (f v)
firstP = postMap (traverse (Just . getFirst))

-- TODO: Move to Data.Vessel.Path
-- | 'Path' that will construct an empty view selector, and get nothing back.
emptyPath :: Monoid m => Path a m m' a'
emptyPath = Path (const mempty) (const Nothing)
