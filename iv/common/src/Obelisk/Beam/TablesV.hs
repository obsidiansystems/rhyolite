{-# LANGUAGE UndecidableInstances #-}
-- | The TablesV structure and associated functionality
module Obelisk.Beam.TablesV
  ( TablesV (..)
  , mapTablesV
  , pureTablesV
  ) where

import Control.Monad.Writer
import Data.Constraint.Compose
import Data.Functor.Const
import Data.Functor.Identity
import Data.Typeable
import Obelisk.Beam.Constraints
import Obelisk.Beam.Constructor
import Obelisk.Beam.DZippable
import Obelisk.Beam.TablesOnly
import Obelisk.Beam.Traversals
import Data.Constraint.Empty (EmptyConstraint)

newtype TablesV db f = TablesV { unTablesV :: db (TableOnly f) }

mapTablesV :: forall db f g. (HasT IsTable db, DZippable db) => (forall a. f a -> g a) -> TablesV db f -> TablesV db g
mapTablesV f = TablesV . mapTablesOnly f . unTablesV

instance (HasT (TableHas Eq f) db, DZippable db) => Eq (TablesV db f) where
  TablesV a == TablesV b = getAll $ execWriter $ zipTablesOnlyWithLeftArgDict @(ComposeC Eq f) (\x y -> Const () <$ tell (All $ x == y)) a b

instance (HasT (TableHas Eq f) db, HasT (TableHas Ord f) db, DZippable db) => Ord (TablesV db f) where
  TablesV a `compare` TablesV b = execWriter $ zipTablesOnlyWithLeftArgDict @(ComposeC Ord f) (\x y -> Const () <$ tell (x `compare` y)) a b

instance (HasT' Semigroup db (TableOnly f), DZippable db) => Semigroup (TablesV db f) where
  TablesV a <> TablesV b = TablesV $ runIdentity $ zipTablesWithLeftArgDict' @Semigroup (\x y -> pure $ x <> y) a b

deriving instance Show (db (TableOnly f)) => Show (TablesV db f)

instance (HasT' Semigroup db (TableOnly f), HasT' Monoid db (TableOnly f), DZippable db) => Monoid (TablesV db f) where
  mappend = (<>)
  mempty = TablesV $ runIdentity $ zipTablesWithLeftArgDict' @Monoid (\_ _ -> pure mempty) e e
    where e = emptyDatabase (undefined :: forall a. TableOnly f a)

pureTablesV
  :: forall db f. (HasT IsTable db, DZippable db)
  => (forall tbl. f tbl)
  -> TablesV db f
pureTablesV v = TablesV $ emptyDatabaseWithArgDict @IsTable e
  where e :: forall e. IsTable e => TableOnly f e
        e = case isTable @e @f of
          Left IsNotTableW -> TableOnly ()
          Right IsTableW -> TableOnly v

instance (ArgDictT db, DZippable db) => ArgDictT (TablesV db) where
  type ConstraintsForT (TablesV db) c = ConstraintsForT db (TableHas_ c)
  hoistWithArgDictT_ (_ :: proxy c) f (TablesV xs) = TablesV $ mapTablesOnlyWithArgDict_ @c f xs

instance (HasT (TableHas_ EmptyConstraint) db, DZippable db) => DMappable (TablesV db) where
  dmap f (TablesV xs) = TablesV <$> traverseTablesOnlyWithArgDict_ @EmptyConstraint f xs

instance (HasT (TableHas_ EmptyConstraint) db, DZippable db) => DZippable (TablesV db) where
  dzip f (TablesV xs) (TablesV ys) = TablesV <$> zipTablesOnlyWithLeftArgDict @EmptyConstraint f xs ys

instance (HasT (TableHas_ EmptyConstraint) db, HasT IsTable db, DPointed db) => DPointed (TablesV db) where
  dpure f = fmap TablesV $ traverseTablesOnlyWithArgDict_ @EmptyConstraint (\_ -> f) $ toTablesOnly $ runIdentity $ dpure $ Identity Proxy
