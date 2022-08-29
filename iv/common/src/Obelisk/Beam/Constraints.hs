{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | ArgDict and related classes for Beam-style records
module Obelisk.Beam.Constraints
  ( ArgDictT (..)
  , HasT
  , HasT'
  , hoistWithArgDictT
  , hoistWithArgDictT'
  , HasCompose
  ) where

import Data.Constraint
import Data.Constraint.Compose
import Data.Proxy
import GHC.Generics
import Data.Vessel (FlipAp(..))
import Data.Constraint.Extras

class ArgDictT f where
  type ConstraintsForT (f :: (k -> *) -> *) (c :: k -> Constraint) :: Constraint
  hoistWithArgDictT_ :: forall c g h proxy. ConstraintsForT f c => proxy c -> (forall a. c a => g a -> h a) -> f g -> f h

hoistWithArgDictT :: forall f c g h. (ArgDictT f, ConstraintsForT f c) => (forall a. c a => g a -> h a) -> f g -> f h
hoistWithArgDictT = hoistWithArgDictT_ (Proxy @c)

type ConstraintsForT' (f :: (k -> *) -> *) (c :: k' -> Constraint) (p :: k -> k') = ConstraintsForT f (ComposeC c p)

hoistWithArgDictT' :: forall f c p g h. HasT' c f p => (forall a. c (p a) => g a -> h a) -> f g -> f h
hoistWithArgDictT' h x = hoistWithArgDictT @f @(ComposeC c p) h x

type HasT c f = (ArgDictT f, ConstraintsForT f c)
type HasT' c f p = (ArgDictT f, ConstraintsForT' f c p)

instance ArgDictT (FlipAp x) where
  type ConstraintsForT (FlipAp x) c = c x
  hoistWithArgDictT_ _ f (FlipAp x) = FlipAp (f x)

instance (ArgDictT a, ArgDictT b) => ArgDictT (a :*: b) where
  type ConstraintsForT (a :*: b) c = (ConstraintsForT a c, ConstraintsForT b c)
  hoistWithArgDictT_ proxy f (a :*: b) = hoistWithArgDictT_ proxy f a :*: hoistWithArgDictT_ proxy f b

class Has c (p x) => HasCompose c p x
instance Has c (p x) => HasCompose c p x

