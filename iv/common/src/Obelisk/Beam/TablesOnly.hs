{-# LANGUAGE UndecidableInstances #-}
-- | Functionality for operating on only the tables within a Beam-style record, skipping other fields (if any)
module Obelisk.Beam.TablesOnly
  ( IsNotTableW (..)
  , IsTable
  , IsTableW (..)
  , TableHas
  , TableHasW (..)
  , TableHas_ (..)
  , TableOnly (..)
  , isTable
  , mapTablesOnly
  , traverseTablesOnly
  , mapTablesOnlyWithArgDict
  , traverseTablesOnlyWithArgDict
  , mapTablesOnlyWithArgDict_
  , traverseTablesOnlyWithArgDict_
  , mapTablesWithArgDict
  , toTablesOnly
  , zipTablesOnly
  , zipTablesOnlyPure
  , zipTablesOnlyWithLeftArgDict
  ) where

import Data.Bifunctor
import Data.Constraint
import Data.Constraint.Compose
import Data.Typeable
import Database.Beam.Schema.Tables
import Obelisk.Beam.Constraints
import Obelisk.Beam.DZippable
import Obelisk.Beam.Traversals
import Data.Functor.Identity
import Data.Functor.Compose

data IsTableW e = forall tbl. e ~ TableEntity tbl => IsTableW
data IsNotTableW f e = (TableT e ~ 'Nothing) => IsNotTableW
newtype SomeIsNotTableW e = SomeIsNotTableW (forall (f :: ((* -> *) -> *) -> *). IsNotTableW f e)

class IsTable e where
  isTable :: forall (f :: ((* -> *) -> *) -> *). Either (IsNotTableW f e) (IsTableW e)

instance IsTable' e (TableT e) => IsTable e where
  isTable = bimap (\(SomeIsNotTableW w) -> w) id (isTable' @e @(TableT e))

class IsTable' e a where
  isTable' :: Either (SomeIsNotTableW e) (IsTableW e)

instance TableT e ~ 'Nothing => IsTable' e 'Nothing where
  isTable' = Left $ SomeIsNotTableW IsNotTableW

instance IsTable' (TableEntity tbl) ('Just tbl) where
  isTable' = Right IsTableW

type TableHas c f = TableHas_ (ComposeC c f)

class TableHas_ c e where
  tableHas :: Either (TableT e :~: 'Nothing) (TableHasW c (TableT e))

instance (TableT e ~ a, TableHas' c a) => TableHas_ c e where
  tableHas = tableHasImpl @c

data TableHasW c a = forall tbl. (MaybeC c a, a ~ 'Just tbl) => TableHasW

class TableHas' c a where
  tableHasImpl :: Either (a :~: 'Nothing) (TableHasW c a)

instance c tbl => TableHas' c ('Just tbl) where
  tableHasImpl = Right TableHasW

instance TableHas' c 'Nothing where
  tableHasImpl = Left Refl

newtype TableOnly f e = TableOnly { unTableOnly :: TableOnlyContents f e }

instance TableHas_ (ComposeC Eq f) e => Eq (TableOnly f e) where
  TableOnly a == TableOnly b = case tableHas @(ComposeC Eq f) @e of
    Left Refl -> True
    Right TableHasW -> a == b

instance TableHas_ (ComposeC Semigroup f) e => Semigroup (TableOnly f e) where
  TableOnly a <> TableOnly b = TableOnly $ case tableHas @(ComposeC Semigroup f) @e of
    Left Refl -> ()
    Right TableHasW -> a <> b

instance (TableHas_ (ComposeC Monoid f) e, TableHas_ (ComposeC Semigroup f) e) => Monoid (TableOnly f e) where
  mempty = TableOnly $ case tableHas @(ComposeC Monoid f) @e of
    Left Refl -> ()
    Right TableHasW -> mempty

instance TableHas_ (ComposeC Show f) e => Show (TableOnly f e) where
  showsPrec n (TableOnly t) = showParen (n > 10) $ showString "TableOnly " . case tableHas @(ComposeC Show f) @e of
    Left Refl -> showsPrec 11 t
    Right TableHasW -> showsPrec 11 t

type family TableT e where
  TableT (TableEntity tbl) = 'Just tbl
  TableT _ = 'Nothing

type family MaybeFT f m where
  MaybeFT f ('Just a) = f a
  MaybeFT _ 'Nothing = ()

type TableOnlyContents f e = MaybeFT f (TableT e)

type family MaybeC c m :: Constraint where
  MaybeC c ('Just a) = c a
  MaybeC _ 'Nothing = ()

zipTablesOnly :: forall db f g h m. (HasT IsTable db, Applicative m, DZippable db) => (forall tbl. f tbl -> g tbl -> m (h tbl)) -> db (TableOnly f) -> db (TableOnly g) -> m (db (TableOnly h))
zipTablesOnly f a b = zipTablesWithLeftArgDict @IsTable g a b
    where g :: forall e. IsTable e => TableOnly f e -> TableOnly g e -> m (TableOnly h e)
          g (TableOnly x) (TableOnly y) = case isTable @e @h of
            Left IsNotTableW -> pure $ TableOnly ()
            Right IsTableW -> TableOnly <$> f x y

zipTablesOnlyPure :: forall db f g h. (HasT IsTable db, DZippable db) => (forall tbl. f tbl -> g tbl -> h tbl) -> db (TableOnly f) -> db (TableOnly g) -> db (TableOnly h)
zipTablesOnlyPure f a b = runIdentity $ zipTablesOnly  (\x y -> pure $ f x y) a b

traverseTablesOnly :: forall db f g m. (HasT IsTable db, Applicative m, DZippable db) => (forall tbl. f tbl -> m (g tbl)) -> db (TableOnly f) -> m (db (TableOnly g))
traverseTablesOnly f a = zipTablesOnly (\_ -> f) a a

mapTablesOnly :: forall db f g. (HasT IsTable db, DZippable db) =>  (forall tbl. f tbl -> g tbl) -> db (TableOnly f) -> db (TableOnly g)
mapTablesOnly f a = runIdentity $ traverseTablesOnly (pure . f) a

mapTablesOnlyWithArgDict
  :: forall (c :: * -> Constraint) p db f g
  .  ( HasT (TableHas c p) db
     , DZippable db
     )
  => (forall tbl. c (p tbl) => f tbl -> g tbl)
  -> db (TableOnly f)
  -> db (TableOnly g)
mapTablesOnlyWithArgDict f = runIdentity . traverseTablesOnlyWithArgDict @c @p  (pure . f)

mapTablesOnlyWithArgDict_
  :: forall (c :: ((* -> *) -> *) -> Constraint) db f g
  .  ( HasT (TableHas_ c) db
     , DZippable db
     )
  => (forall tbl. c tbl => f tbl -> g tbl)
  -> db (TableOnly f)
  -> db (TableOnly g)
mapTablesOnlyWithArgDict_ f = runIdentity . traverseTablesOnlyWithArgDict_ @c (pure . f)

traverseTablesOnlyWithArgDict
  :: forall (c :: * -> Constraint) p db f g m
  .  ( HasT (TableHas c p) db
     , Applicative m
     , DZippable db
     )
  => (forall tbl. c (p tbl) => f tbl -> m (g tbl))
  -> db (TableOnly f)
  -> m (db (TableOnly g))
traverseTablesOnlyWithArgDict f = traverseTablesWithArgDict @(TableHas c p) g
  where g :: forall e. (TableHas c p e) => TableOnly f e -> m (TableOnly g e)
        g (TableOnly x) = case tableHas @(ComposeC c p) @e of
            Left Refl -> pure $ TableOnly ()
            Right TableHasW -> TableOnly <$> f x

traverseTablesOnlyWithArgDict_
  :: forall (c :: ((* -> *) -> *) -> Constraint) db f g m
  .  ( HasT (TableHas_ c) db
     , Applicative m
     , DZippable db
     )
  => (forall tbl. c tbl => f tbl -> m (g tbl))
  -> db (TableOnly f)
  -> m (db (TableOnly g))
traverseTablesOnlyWithArgDict_ f = traverseTablesWithArgDict @(TableHas_ c) g
  where g :: forall e. (TableHas_ c e) => TableOnly f e -> m (TableOnly g e)
        g (TableOnly x) = case tableHas @c @e of
            Left Refl -> pure $ TableOnly ()
            Right TableHasW -> TableOnly <$> f x

toTablesOnly :: forall db f. (HasT IsTable db, DZippable db) => db f -> db (TableOnly (Compose f TableEntity))
toTablesOnly = mapTablesWithArgDict @IsTable f
  where f :: forall e. IsTable e => f e -> TableOnly (Compose f TableEntity) e
        f a = case isTable @e @(Compose f TableEntity) of
          Left IsNotTableW -> TableOnly ()
          Right IsTableW -> TableOnly $ Compose a

zipTablesOnlyWithLeftArgDict
  :: forall (c :: ((* -> *) -> *) -> Constraint) db f g h m
  .  ( HasT (TableHas_ c) db
     , Applicative m
     , DZippable db
     )
  => (forall tbl. c tbl => f tbl -> g tbl -> m (h tbl))
  -> db (TableOnly f)
  -> db (TableOnly g)
  -> m (db (TableOnly h))
zipTablesOnlyWithLeftArgDict f = zipTablesWithLeftArgDict @(TableHas_ c) g
  where g :: forall e. (TableHas_ c e) => TableOnly f e -> TableOnly g e -> m (TableOnly h e)
        g (TableOnly x) (TableOnly y) = case tableHas @c @e of
            Left Refl -> pure $ TableOnly ()
            Right TableHasW -> TableOnly <$> f x y

