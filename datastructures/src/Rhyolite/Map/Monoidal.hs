{-# LANGUAGE ScopedTypeVariables #-}
module Rhyolite.Map.Monoidal (module X, (=:), restrictKeys) where

import Data.AppendMap as X
import Data.Map.Monoidal as X

import Data.Coerce (coerce)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Operator for creating a singleton 'Map'
(=:) :: k -> a -> MonoidalMap k a
k =: v = singleton k v
infixr 7 =:

restrictKeys :: forall k v. Ord k => MonoidalMap k v -> Set.Set k -> MonoidalMap k v
restrictKeys = coerce (Map.restrictKeys :: Map.Map k v -> Set.Set k -> Map.Map k v)
