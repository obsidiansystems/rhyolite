module Obelisk.View.Interface where

import Prelude hiding (id, (.))

import Obelisk.View.Coverable
import Obelisk.View.Coverage
import Data.Constraint.Forall
import Data.Functor.Const
import Data.Map (Map)
import Data.IntMap (IntMap)

-- | The type parameter `a` will generally be `Type`, but sometimes it will be `Type -> Type` or something more exotic - we use it a bit like an applicative functor at the type level.
data Interface a = Interface
  a -- Push
  a -- Pull

-- | Specifies a way of updating a subset of the possible data
type family Push (a :: Interface k) where
  Push ('Interface a _) = a

-- | Contains a subset of the possible data
type family Pull (a :: Interface k) where
  Pull ('Interface _ a) = a

--------------------------------------------------------------------------------
-- Convenience functions and types
--------------------------------------------------------------------------------

covCommutesPushPull
  :: forall i a x. (Forall (CovFunctor (Push i)), Forall (CovFunctor (Pull i)))
  => ((CovFunctor (Push i) a, CovFunctor (Pull i) a) => x)
  -> x
covCommutesPushPull x = covCommutes @(Push i) @a $ covCommutes @(Pull i) @a $ x

type ApInterface (f :: Interface (k -> k')) (a :: Interface k) = 'Interface (Push f (Push a)) (Pull f (Pull a))

type LiftInterface f = 'Interface f f

type ConstInterface (i :: Interface *) = 'Interface (Const (Push i)) (Const (Pull i))

type ShowInterface a =
  ( Show (Cov (Push a))
  , Show (Push a)
  , Show (Cov (Pull a))
  , Show (WithFullCoverage (Cov (Push a)))
  , Show (WithFullCoverage (Cov (Pull a)))
  , Show (Pull a)
  )

type MapInterface k i = ApInterface (LiftInterface (Map k)) i

type IntMapInterface i = ApInterface (LiftInterface (IntMap)) i
