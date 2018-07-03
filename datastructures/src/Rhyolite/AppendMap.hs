module Rhyolite.AppendMap (module X, (=:), restrictKeys) where

import Data.AppendMap as X

import qualified Data.Set as Set

-- | Operator for creating a singleton 'Map'
(=:) :: k -> a -> AppendMap k a
k =: v = singleton k v
infixr 7 =:

-- TODO: Use built-in implementation after upgrading 'containers'.
restrictKeys :: Ord k => AppendMap k a -> Set.Set k -> AppendMap k a
restrictKeys m ks = filterWithKey (\k _ -> k `Set.member` ks) m
