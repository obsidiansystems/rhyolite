-- | Utilities for dealing with 'MonoidalMap's.

module Rhyolite.Map.Monoidal (module X, (=:), restrictKeys) where

import Data.AppendMap as X
import Data.Map.Monoidal as X

import qualified Data.Set as Set

-- | Operator for creating a singleton 'Map'
(=:) :: k -> a -> MonoidalMap k a
k =: v = singleton k v
infixr 7 =:

-- TODO: Use built-in implementation after upgrading 'containers'.
-- | Produce a monoidal map with only the specified keys
restrictKeys :: Ord k => MonoidalMap k a -> Set.Set k -> MonoidalMap k a
restrictKeys m ks = filterWithKey (\k _ -> k `Set.member` ks) m
