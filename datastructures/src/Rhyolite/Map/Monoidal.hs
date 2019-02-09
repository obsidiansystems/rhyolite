{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Rhyolite.Map.Monoidal (module X, (=:), restrictKeys) where

import Data.Align
import Data.AppendMap as X
import Data.Map.Monoidal as X

import qualified Data.Set as Set

-- | Operator for creating a singleton 'Map'
(=:) :: k -> a -> MonoidalMap k a
k =: v = singleton k v
infixr 7 =:

-- TODO: Use built-in implementation after upgrading 'containers'.
restrictKeys :: Ord k => MonoidalMap k a -> Set.Set k -> MonoidalMap k a
restrictKeys m ks = filterWithKey (\k _ -> k `Set.member` ks) m

#if !(MIN_VERSION_monoidal_containers(0,4,1))
deriving instance Ord k => Align (MonoidalMap k)
#endif
