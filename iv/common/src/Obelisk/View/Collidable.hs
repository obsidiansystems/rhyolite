{-# LANGUAGE UndecidableInstances #-}
module Obelisk.View.Collidable where

import GHC.Stack
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Functor.Misc
import Data.Proxy
import Data.These
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Monoid
import Data.Functor.Const
import Data.Map.Strict (Map)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntMap.Merge.Strict as IntMap
import Data.GADT.Compare
import Data.Constraint.Extras
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Obelisk.View.These1
import Data.Bifunctor
import Data.Either.Combinators
import Obelisk.View.Misc
import Data.Void

class (Semigroup (Collision a)) => Collidable a where
  type Collision a :: *
  mergeDisjoint :: a -> a -> Either (Collision a) a

--------------------------------------------------------------------------------
-- Convenience functions
--------------------------------------------------------------------------------

mergeAssertDisjoint :: (HasCallStack, Collidable a, Show (Collision a)) => a -> a -> a
mergeAssertDisjoint a b = either (error . ("collision: "<>) . show) id $ mergeDisjoint a b

mergeMaybesDisjoint :: Collidable a => Maybe a -> Maybe a -> Either (Collision a) (Maybe a)
mergeMaybesDisjoint ma mb = case (ma, mb) of
  (Nothing, Nothing) -> Right Nothing
  (Just a, Nothing) -> Right $ Just a
  (Nothing, Just b) -> Right $ Just b
  (Just a, Just b) -> Just <$> mergeDisjoint a b

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

-- ()

instance Collidable () where
  type Collision () = Sum Int -- Number of collisions
  mergeDisjoint _ _ = Left $ Sum 1

-- TODO: Organize these

deriving instance Collidable a => Collidable (Identity a)

deriving instance Collidable (f (g a)) => Collidable (Compose f g a)
deriving instance Collidable (Maybe (g a)) => Collidable (ComposeMaybe g a)

instance Collidable a => Collidable (Maybe a) where
  type Collision (Maybe a) = Collision a
  mergeDisjoint ma mb = case (ma, mb) of
    (Nothing, Nothing) -> Right Nothing
    (Just a, Nothing) -> Right $ Just a
    (Nothing, Just b) -> Right $ Just b
    (Just a, Just b) -> Just <$> mergeDisjoint a b

instance (Collidable a, Collidable b) => Collidable (These a b) where
  type Collision (These a b) = These (Collision a) (Collision b)
  mergeDisjoint = \case
    This a1 -> \case
      This b1 -> bimap This This $ mergeDisjoint a1 b1
      These b1 b2 -> case mergeDisjoint a1 b1 of
        Left c1 -> Left $ This c1
        Right c1 -> Right $ These c1 b2
      That b2 -> Right $ These a1 b2
    These a1 a2 -> \case
      This b1 -> case mergeDisjoint a1 b1 of
        Left c1 -> Left $ This c1
        Right c1 -> Right $ These c1 a2
      These b1 b2 -> case (mergeDisjoint a1 b1, mergeDisjoint a2 b2) of
        (Left c1, Left c2) -> Left $ These c1 c2
        (Left c1, Right _) -> Left $ This c1
        (Right _, Left c2) -> Left $ That c2
        (Right c1, Right c2) -> Right $ These c1 c2
      That b2 -> case mergeDisjoint a2 b2 of
        Left c2 -> Left $ That c2
        Right c2 -> Right $ These a1 c2
    That a2 -> \case
      This b1 -> Right $ These b1 a2
      These b1 b2 -> case mergeDisjoint a2 b2 of
        Left c2 -> Left $ That c2
        Right c2 -> Right $ These b1 c2
      That b2 -> case mergeDisjoint a2 b2 of
        Left c2 -> Left $ That c2
        Right c2 -> Right $ That c2

instance Collidable Int where
  type Collision Int = Seq Int
  mergeDisjoint a b = Left $ Seq.fromList [a, b]

instance Collidable a => Collidable (Sum a) where
  type Collision (Sum a) = Seq (Collision a)
  mergeDisjoint (Sum a) (Sum b) = bimap Seq.singleton Sum $ mergeDisjoint a b

instance Collidable a => Collidable (Const a b) where
  type Collision (Const a b) = Seq (Collision a)
  mergeDisjoint (Const a) (Const b) = bimap Seq.singleton Const $ mergeDisjoint a b

instance Collidable (Proxy b) where
  type Collision (Proxy b) = Sum Int
  mergeDisjoint _ _ = Left $ Sum 1

instance Collidable a => Collidable (IntMap a) where
  type Collision (IntMap a) = IntMap (Collision a)
  mergeDisjoint a b = if IntMap.null collisions then Right noncollisions else Left collisions
    where merged = IntMap.merge (IntMap.mapMissing $ const Right) (IntMap.mapMissing $ const Right) (IntMap.zipWithMatched (\_ -> mergeDisjoint)) a b
          collisions :: IntMap (Collision a)
          collisions = IntMap.mapMaybe leftToMaybe merged
          noncollisions = IntMap.mapMaybe rightToMaybe merged

instance (GCompare k, Has' Collidable k v) => Collidable (DMap k v) where
  type Collision (DMap k v) = DMap k (Compose Collision' v) -- TODO: Should be MonoidalDMap!
  mergeDisjoint = alignDMapWithKeyM g --TODO: Use Validation to accumulate collisions
    where g :: forall a. k a -> These1 v v a -> Either (DMap k (Compose Collision' v)) (v a)
          g k = \case
            These1 x y -> has' @Collidable @v k $ case mergeDisjoint x y of
              Left c -> Left $ DMap.singleton k $ Compose $ Collision' c
              Right z -> Right z
            This1 x -> Right x
            That1 y -> Right y

newtype Collision' a = Collision' (Collision a)

deriving instance Show (Collision a) => Show (Collision' a)

-- Map

instance (Collidable v, Ord k) => Collidable (Map k v) where
  type Collision (Map k v) = Map k (Collision v)
  mergeDisjoint = mergeMapDisjointWith mergeDisjoint

instance Collidable Void where
  type Collision Void = Void
  mergeDisjoint = absurd
