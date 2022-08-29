-- | Zipping for Beam-style records without `Beamable`
module Obelisk.Beam.DZippable
  ( DZippable (..)
  , DMappable (..)
  , DPointed (..)
  , zipDBMaybe
  , alignDBMaybe
  , mapDBMaybe
  ) where

import qualified GHC.Generics as Generic
import GHC.Generics hiding (R, C)
import Data.Proxy
import Control.Applicative
import Data.Vessel (FlipAp(..))
import Data.Functor.Misc
import Data.These
import Data.These.Combinators
import Data.Monoid (Any(..))
import Control.Monad.Writer.CPS
import Data.Align
import Data.Maybe (isJust)

class DMappable db where

  dmap :: Applicative m
       => (forall tbl. f tbl -> m (h tbl))
       -> db f -> m (db h)
  default dmap :: ( Generic (db f), Generic (db h)
                        , Applicative m
                        , GMapDatabase' f h
                                      (Rep (db f)) (Rep (db h)) ) =>
                        (forall tbl. f tbl -> m (h tbl)) ->
                        db f -> m (db h)
  -- We need the pattern type signature on 'combine' to get around a type checking bug in GHC 8.0.1. In future releases,
  -- we will switch to the standard forall.
  dmap combine (f :: db f) =
    refl' $ \h ->
      to <$> gMapDatabase' (Proxy @f, h) combine (from f)
    where
      -- For GHC 8.0.1 renamer bug
      refl' :: (Proxy h -> m (db h)) -> m (db h)
      refl' fn = fn Proxy


class GMapDatabase' f h x z where
  gMapDatabase' :: Applicative m =>
                  (Proxy f, Proxy h)
               -> (forall tbl. f tbl -> m (h tbl))
               -> x () -> m (z ())
instance GMapDatabase' f h x z =>
  GMapDatabase' f h (M1 a b x) (M1 a b z) where
  gMapDatabase' p combine ~(M1 f) = M1 <$> gMapDatabase' p combine f
instance ( GMapDatabase' f h ax az
         , GMapDatabase' f h bx bz ) =>
  GMapDatabase' f h (ax :*: bx) (az :*: bz) where
  gMapDatabase' p combine ~(ax :*: bx) =
    liftA2 (:*:) (gMapDatabase' p combine ax) (gMapDatabase' p combine bx)
instance GMapDatabase' f h (K1 Generic.R (f tbl)) (K1 Generic.R (h tbl)) where

  gMapDatabase' _ combine ~(K1 x) =
    K1 <$> combine x

instance DMappable db =>
  GMapDatabase' f h (K1 Generic.R (db f)) (K1 Generic.R (db h)) where

  gMapDatabase' _ combine ~(K1 x) =
    K1 <$> dmap combine x


class DMappable db => DZippable db where

  dzip :: Applicative m
       => (forall tbl. f tbl -> g tbl -> m (h tbl))
       -> db f -> db g -> m (db h)
  default dzip :: ( Generic (db f), Generic (db g), Generic (db h)
                        , Applicative m
                        , GZipDatabase' f g h
                                      (Rep (db f)) (Rep (db g)) (Rep (db h)) ) =>
                        (forall tbl. f tbl -> g tbl -> m (h tbl)) ->
                        db f -> db g -> m (db h)
  -- We need the pattern type signature on 'combine' to get around a type checking bug in GHC 8.0.1. In future releases,
  -- we will switch to the standard forall.
  dzip combine (f :: db f) (g :: db g) =
    refl' $ \h ->
      to <$> gZipDatabase' (Proxy @f, Proxy @g, h) combine (from f) (from g)
    where
      -- For GHC 8.0.1 renamer bug
      refl' :: (Proxy h -> m (db h)) -> m (db h)
      refl' fn = fn Proxy

class GZipDatabase' f g h x y z where
  gZipDatabase' :: Applicative m =>
                  (Proxy f, Proxy g, Proxy h)
               -> (forall tbl. f tbl -> g tbl -> m (h tbl))
               -> x () -> y () -> m (z ())
instance GZipDatabase' f g h x y z =>
  GZipDatabase' f g h (M1 a b x) (M1 a b y) (M1 a b z) where
  gZipDatabase' p combine ~(M1 f) ~(M1 g) = M1 <$> gZipDatabase' p combine f g
instance ( GZipDatabase' f g h ax ay az
         , GZipDatabase' f g h bx by bz ) =>
  GZipDatabase' f g h (ax :*: bx) (ay :*: by) (az :*: bz) where
  gZipDatabase' p combine ~(ax :*: bx) ~(ay :*: by) =
    liftA2 (:*:) (gZipDatabase' p combine ax ay) (gZipDatabase' p combine bx by)
instance GZipDatabase' f g h (K1 Generic.R (f tbl)) (K1 Generic.R (g tbl)) (K1 Generic.R (h tbl)) where

  gZipDatabase' _ combine ~(K1 x) ~(K1 y) =
    K1 <$> combine x y

instance DZippable db =>
  GZipDatabase' f g h (K1 Generic.R (db f)) (K1 Generic.R (db g)) (K1 Generic.R (db h)) where

  gZipDatabase' _ combine ~(K1 x) ~(K1 y) =
    K1 <$> dzip combine x y

class DZippable db => DPointed db where

  dpure :: Applicative m
       => (forall tbl. m (h tbl))
       -> m (db h)
  default dpure :: ( Generic (db h)
                        , Applicative m
                        , GPointDatabase' h
                                      (Rep (db h)) ) =>
                        (forall tbl. m (h tbl)) ->
                        m (db h)
  -- We need the pattern type signature on 'combine' to get around a type checking bug in GHC 8.0.1. In future releases,
  -- we will switch to the standard forall.
  dpure combine =
    refl' $ \h ->
      to <$> gPointDatabase' h combine
    where
      -- For GHC 8.0.1 renamer bug
      refl' :: (Proxy h -> m (db h)) -> m (db h)
      refl' fn = fn Proxy


class GPointDatabase' h z where
  gPointDatabase' :: Applicative m =>
                  (Proxy h)
               -> (forall tbl. m (h tbl))
               -> m (z ())
instance GPointDatabase' h z =>
  GPointDatabase' h (M1 a b z) where
  gPointDatabase' p combine = M1 <$> gPointDatabase' p combine
instance ( GPointDatabase' h az
         , GPointDatabase' h bz ) =>
  GPointDatabase' h (az :*: bz) where
  gPointDatabase' p combine =
    liftA2 (:*:) (gPointDatabase' p combine ) (gPointDatabase' p combine )
instance GPointDatabase' h (K1 Generic.R (h tbl)) where

  gPointDatabase' _ combine =
    K1 <$> combine

instance DPointed db =>
  GPointDatabase' h (K1 Generic.R (db h)) where

  gPointDatabase' _ combine =
    K1 <$> dpure combine

instance (DMappable f, DMappable g) => DMappable (f :*: g) where
  dmap f (xs :*: ys) = (:*:) <$> dmap f xs <*> dmap f ys
instance DMappable (FlipAp a) where
  dmap f (FlipAp a) = FlipAp <$> f a
instance DMappable Proxy where
  dmap _ Proxy = pure Proxy
instance DMappable (Const a) where
  dmap _ (Const x) = pure $ Const x

instance (DZippable f, DZippable g) => DZippable (f :*: g) where
  dzip f (xs :*: ys) (xs' :*: ys') = (:*:) <$> dzip f xs xs' <*> dzip f ys ys'
instance DZippable (FlipAp a) where
  dzip f (FlipAp a) (FlipAp b) = FlipAp <$> f a b
instance DZippable Proxy where
  dzip _ Proxy Proxy = pure Proxy
instance Semigroup a => DZippable (Const a) where
  dzip _ (Const x) (Const y) = pure $ Const $ x <> y

instance (DPointed f, DPointed g) => DPointed (f :*: g) where
  dpure f = (:*:) <$> dpure f <*> dpure f
instance DPointed (FlipAp a) where
  dpure f = FlipAp <$> f
instance DPointed Proxy where
  dpure _ = pure Proxy
instance Monoid a => DPointed (Const a) where
  dpure _ = pure $ Const mempty

zipDBMaybe :: DZippable db => (forall x. Maybe (f x) -> Maybe (g x) -> Maybe (h x)) -> db (ComposeMaybe f) -> db (ComposeMaybe g) -> Maybe (db (ComposeMaybe h))
zipDBMaybe f = alignDBMaybe (\z -> f (justHere z) (justThere z))

alignDBMaybe :: DZippable db => (forall x. These (f x) (g x) -> Maybe (h x)) -> db (ComposeMaybe f) -> db (ComposeMaybe g) -> Maybe (db (ComposeMaybe h))
alignDBMaybe f xs ys = if anyJust then Just zs else Nothing
  where
    (zs, Any anyJust) = runWriter $ dzip (\(ComposeMaybe x) (ComposeMaybe y) ->
      let z = f =<< align x y in ComposeMaybe z <$ tell (Any $ isJust z)) xs ys

mapDBMaybe :: DMappable db => (forall x. f x -> Maybe (g x)) -> db (ComposeMaybe f) -> Maybe (db (ComposeMaybe g))
mapDBMaybe f xs = if anyJust then Just ys else Nothing
  where
    (ys, Any anyJust) = runWriter $ dmap (\(ComposeMaybe x) ->
      let y = f =<< x in ComposeMaybe y <$ tell (Any $ isJust y)) xs

