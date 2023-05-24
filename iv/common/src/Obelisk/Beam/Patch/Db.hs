{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- TODO Eliminate this
{-# OPTIONS_GHC -Werror=missing-methods #-} -- TODO Eliminate this


module Obelisk.Beam.Patch.Db
  ( DbPatchV (..)
  , BeamableOrdPrimaryKey
  , allDbPatchV
  , emptyDbPatchV
  , pureDbPatchV

  , QueryResultPatch(..), _QueryResultPatch
  , nonEmptyQueryResultPatch, mapQueryResultPatchMaybe
  ) where

import Control.Applicative
import Control.Lens (makePrisms)
import Control.Lens ((.~), Lens', view)
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Align
import Data.Foldable
import Data.Functor.Identity
import Data.Functor.Misc
import Data.Maybe (fromMaybe, isNothing)
import Data.Semigroup.Commutative
import Data.Sequence ((|>), Seq(..))
import Data.These
import Data.Vessel
import Data.Witherable
import Database.Beam.Schema.Tables
import Obelisk.Beam.BLens
import Obelisk.Beam.Constraints
import Obelisk.Beam.Constructor
import Obelisk.Beam.DZippable
import Obelisk.Beam.Patch.Table
import Obelisk.Beam.TablesOnly
import Obelisk.Beam.TablesV
import Obelisk.Beam.Traversals
import Obelisk.Beam.View.Db
import Obelisk.Beam.View.Table
import Obelisk.View.Misc
import Reflex.Query.Class
import Obelisk.View.Coverable
import Obelisk.View.Coverage

import Data.Constraint.Compose
import Data.Patch

newtype DbPatchV db p f = DbPatchV { unDbPatchV :: DbPatchVInternal db p f }

type DbPatchVInternal db p f = TablesV db (ComposeMaybe (Compose f p))

class (Beamable tbl, Ord (PrimaryKey tbl Identity)) => BeamableOrdPrimaryKey tbl
instance (Beamable tbl, Ord (PrimaryKey tbl Identity)) => BeamableOrdPrimaryKey tbl

instance ( HasT' Semigroup db (TableOnly (ComposeMaybe (Compose Identity TablePatch)))
         , DZippable db
         , HasT IsTable db
         , HasT (TableHas_ BeamableOrdPrimaryKey) db
         ) => Patch (DbPatchV db TablePatch Identity) where
  type PatchTarget (DbPatchV db TablePatch Identity) = DbView db
  --TODO: return Nothing sometimes
  apply (DbPatchV (TablesV p)) (DbView (TablesV d)) = Just $ DbView $ TablesV $ runIdentity $ zipTablesOnlyWithLeftArgDict @BeamableOrdPrimaryKey f p d
    where f :: forall tbl. BeamableOrdPrimaryKey tbl => ComposeMaybe (Compose Identity TablePatch) tbl -> TableView tbl -> Identity (TableView tbl)
          f (ComposeMaybe mtp) tv = pure $ case mtp of
            Nothing -> tv
            Just (Compose (Identity tp)) -> applyAlways tp tv

instance ( HasT' Semigroup db (TableOnly (ComposeMaybe (Compose f p)))
         , DZippable db
         ) => Semigroup (DbPatchV db p f) where
  DbPatchV (TablesV a) <> DbPatchV (TablesV b) = DbPatchV $ TablesV $ runIdentity $ zipTablesWithLeftArgDict' @Semigroup (\x y -> pure $ x <> y) a b

instance ( HasT' Semigroup db (TableOnly (ComposeMaybe (Compose f p)))
         , DZippable db
         , HasT IsTable db
         ) => Monoid (DbPatchV db p f) where
  mempty = emptyDbPatchV

instance ( DZippable db
         , HasBLenses db
         , HasT' Semigroup db (TableOnly (ComposeMaybe (Compose Identity p)))
         , HasT' Monoid db (TableOnly (ComposeMaybe (Compose Identity p)))
         , HasT IsTable db
         ) => Reflex.Query.Class.Query (DbPatchV db (p :: ((* -> *) -> *) -> *) (Const ())) where
  type QueryResult (DbPatchV db p (Const ())) = DbPatchV db p Identity
  crop q r = fromMaybe mempty $ cropV (\_ v -> v) q r

instance ( HasT' Semigroup db (TableOnly (ComposeMaybe (Compose (Const SelectedCount) p)))
         , DZippable db
         ) => Commutative (DbPatchV db p (Const SelectedCount))

deriving instance Show (db (TableOnly (ComposeMaybe (Compose f p)))) => Show (DbPatchV db p f)
deriving instance (DZippable db, HasT (TableHas Eq (ComposeMaybe (Compose f p))) db) => Eq (DbPatchV db p f)

instance ( HasBLenses db
         , HasT IsTable db
         , DZippable db
         ) => View (DbPatchV db p) where
  condenseV (m :: t (DbPatchV db p f)) = DbPatchV $ TablesV $ runIdentity $ zipTablesWithLeftArgDict @IsTable (\(BLens l) _ -> pure $ f l) blenses blenses
    where f :: forall e. IsTable e => Lens' (db (TableOnly (ComposeMaybe (Compose f p)))) (TableOnly (ComposeMaybe (Compose f p)) e) -> TableOnly (ComposeMaybe (Compose (Compose t f) p)) e
          f l = case isTable @e @(ComposeMaybe (Compose (Compose t f) p)) of
            Left IsNotTableW -> TableOnly ()
            Right IsTableW -> TableOnly $ ComposeMaybe $ fmap (Compose . Compose) $ nothingIfNull $ mapMaybe (\(DbPatchV (TablesV d)) -> fmap getCompose $ getComposeMaybe $ unTableOnly (view l d :: TableOnly (ComposeMaybe (Compose f p)) e)) m
  disperseV (DbPatchV (TablesV db) :: DbPatchV db p (Compose t f)) = execState (zipTablesWithLeftArgDict @IsTable f db blenses :: State (t (DbPatchV db p f)) (db (Const ()))) (nil :: t (DbPatchV db p f))
    where f :: forall e. IsTable e => TableOnly (ComposeMaybe (Compose (Compose t f) p)) e -> BLens db e -> State (t (DbPatchV db p f)) (Const () e)
          f (TableOnly v) (BLens l) = case isTable @e @(ComposeMaybe (Compose (Compose t f) p)) of
            Left IsNotTableW -> pure $ Const ()
            Right IsTableW -> do
              forM_ (getComposeMaybe v) $ \(Compose (Compose x)) -> do
                let nothingIfTable :: forall e' f'. IsTable e' => TableOnly (ComposeMaybe f') e'
                    nothingIfTable = case isTable @e' @f' of
                      Left IsNotTableW -> TableOnly ()
                      Right IsTableW -> TableOnly $ ComposeMaybe Nothing
                    g = DbPatchV . TablesV . \case
                      This a -> (l .~ TableOnly (ComposeMaybe $ Just $ Compose a)) $ emptyDatabaseWithArgDict @IsTable nothingIfTable
                      That (DbPatchV (TablesV db')) -> db'
                      These a (DbPatchV (TablesV db')) -> (l .~ TableOnly (ComposeMaybe $ Just $ Compose a)) db'
                modify $ alignWith g x
              pure $ Const ()
  cropV f (DbPatchV (TablesV a)) (DbPatchV (TablesV b)) = collapseNullV $ DbPatchV $ TablesV $ runIdentity $ zipTablesOnly  (\(ComposeMaybe x) (ComposeMaybe y) -> pure $ ComposeMaybe $ liftA2 (\(Compose x') (Compose y') -> Compose $ f x' y') x y) a b
  nullV (DbPatchV (TablesV v)) = getAll $ execWriter $ zipTablesOnly  (\(ComposeMaybe x) _ -> fmap Const $ tell $ All $ isNothing x) v v
  mapV f (DbPatchV (TablesV v)) = DbPatchV $ TablesV $ runIdentity $ zipTablesOnly  (\(ComposeMaybe mx) _ -> pure $ ComposeMaybe $ fmap (\(Compose x) -> Compose $ f x) mx) v v
  traverseV f (DbPatchV (TablesV v)) = DbPatchV . TablesV <$> zipTablesOnly  (\(ComposeMaybe mx) _ -> ComposeMaybe <$> traverse (\(Compose x) -> Compose <$> f x) mx) v v
  mapMaybeV :: forall f g. (forall a. f a -> Maybe (g a)) -> DbPatchV db p f -> Maybe (DbPatchV db p g)
  mapMaybeV f (DbPatchV (TablesV v)) = if isEmpty then Nothing else Just $ DbPatchV $ TablesV v'
    where (v', All isEmpty) = runWriter $ traverseTablesOnly  g v
          g :: forall tbl. ComposeMaybe (Compose f p) tbl -> Writer All (ComposeMaybe (Compose g p) tbl)
          g (ComposeMaybe mx) = ComposeMaybe <$> case f . getCompose =<< mx of
            Nothing -> pure Nothing
            Just y -> do
              tell $ All False
              pure $ Just $ Compose y
  alignWithV (f :: forall a. These (f a) (g a) -> h a) a b = fromMaybe emptyDbPatchV $ alignWithMaybeV (\t -> Just $ f t) a b
  alignWithMaybeV (f :: forall tbl. These (f (p tbl)) (g (p tbl)) -> Maybe (h (p tbl))) (DbPatchV (TablesV a)) (DbPatchV (TablesV b)) = collapseNullV $ DbPatchV $ TablesV $ zipTablesOnlyPure g a b
    where g :: forall tbl. ComposeMaybe (Compose f p) tbl -> ComposeMaybe (Compose g p) tbl -> ComposeMaybe (Compose h p) tbl
          g (ComposeMaybe mx) (ComposeMaybe my) = ComposeMaybe $ fmap Compose $ case (mx, my) of
            (Just (Compose x), Just (Compose y)) -> f $ These x y
            (Just (Compose x), Nothing) -> f $ This x
            (Nothing, Just (Compose y)) -> f $ That y
            (Nothing, Nothing) -> Nothing

instance ( HasT' Semigroup db (TableOnly (ComposeMaybe (Compose (Const SelectedCount) p)))
         , HasT' Monoid db (TableOnly (ComposeMaybe (Compose (Const SelectedCount) p)))
         , HasT IsTable db
         , DZippable db
         ) => Group (DbPatchV db p (Const SelectedCount)) where
  negateG (DbPatchV (TablesV db)) = DbPatchV $ TablesV $ mapTablesOnly  (\(ComposeMaybe mx) -> ComposeMaybe $ fmap (Compose . negateG . getCompose) mx) db

instance ( HasBLenses db
         , HasT' Semigroup db (TableOnly (ComposeMaybe (Compose Identity p)))
         , HasT' Monoid db (TableOnly (ComposeMaybe (Compose Identity p)))
         , HasT IsTable db
         , DZippable db
         ) => Reflex.Query.Class.Query (DbPatchV db (p :: ((* -> *) -> *) -> *) (Const SelectedCount)) where
  type QueryResult (DbPatchV db p (Const SelectedCount)) = DbPatchV db p Identity
  crop q r = fromMaybe mempty $ cropV (\_ v -> v) q r --TODO

instance (HasT (TableHas ToJSON (Compose f p)) db, DZippable db) => ToJSON (DbPatchV db p f) where
  toJSON (DbPatchV (TablesV db)) = toJSON $ toList $ execState (traverseTablesOnlyWithArgDict @ToJSON @(Compose f p)  f db) mempty
    where f :: ToJSON (Compose f p tbl) => ComposeMaybe (Compose f p) tbl -> State (Seq Value) (Const () tbl)
          f (ComposeMaybe a) = do
            modify $ (|> toJSON a)
            pure $ Const ()

instance(HasT IsTable db, HasT (TableHas FromJSON (Compose f p)) db, DZippable db) =>  FromJSON (DbPatchV db p f) where
  parseJSON v = do
    tableVals <- parseJSON v
    let DbPatchV (TablesV emptyDb) = emptyDbPatchV
    DbPatchV . TablesV <$> evalStateT (traverseTablesOnlyWithArgDict @FromJSON @(Compose f p)  f emptyDb) tableVals
    where f :: FromJSON (Compose f p tbl) => ComposeMaybe (Compose (Const ()) p) tbl -> StateT (Seq Value) Parser (ComposeMaybe (Compose f p) tbl)
          f (ComposeMaybe _) = do
            h :<| t <- get
            put t
            ComposeMaybe <$> lift (parseJSON h)

pureDbPatchV
  :: forall db p f. (HasT IsTable db, DZippable db)
  => (forall tbl. Maybe (Compose f p tbl))
  -> DbPatchV db p f
pureDbPatchV v = DbPatchV $ pureTablesV $ ComposeMaybe v

emptyDbPatchV :: forall db p f. (HasT IsTable db, DZippable db) => DbPatchV db p f
emptyDbPatchV = pureDbPatchV Nothing

allDbPatchV :: forall db p. (HasT IsTable db, DZippable db) => DbPatchV db p (Const ())
allDbPatchV = pureDbPatchV $ Just $ Compose $ Const ()

newtype QueryResultPatch tbls p = QueryResultPatch { unQueryResultPatch :: tbls (ComposeMaybe p) }

mapQueryResultPatchMaybe :: DMappable tbls => (forall tbl. f tbl -> Maybe (g tbl)) -> QueryResultPatch tbls f -> Maybe (QueryResultPatch tbls g)
mapQueryResultPatchMaybe f (QueryResultPatch xs) = QueryResultPatch <$> mapDBMaybe f xs

nonEmptyQueryResultPatch :: DMappable tbls => QueryResultPatch tbls f -> Maybe (QueryResultPatch tbls f)
nonEmptyQueryResultPatch = mapQueryResultPatchMaybe Just



deriving instance Eq (tbls (ComposeMaybe p)) => Eq (QueryResultPatch tbls p)
deriving instance Show (tbls (ComposeMaybe p)) => Show (QueryResultPatch tbls p)

instance forall p tbls. (DZippable tbls, HasT (ComposeC Semigroup p) tbls) => Semigroup (QueryResultPatch tbls p) where

  QueryResultPatch xs <> QueryResultPatch ys = QueryResultPatch $ runIdentity $ zipTablesWithLeftArgDict @(ComposeC Semigroup p) combine xs ys
    where
      combine :: forall tbl. Semigroup (p tbl) => ComposeMaybe p tbl -> ComposeMaybe p tbl -> Identity (ComposeMaybe p tbl)
      combine (ComposeMaybe x) (ComposeMaybe y) = Identity $ ComposeMaybe $ alignWith (these id id (<>)) x y
instance forall p tbls. (DPointed tbls, HasT (ComposeC Semigroup p) tbls) => Monoid (QueryResultPatch tbls p) where
  mappend = (<>)
  mempty = QueryResultPatch $ runIdentity $ dpure $ Identity $ ComposeMaybe Nothing


makePrisms ''QueryResultPatch

instance HasCov (QueryResultPatch tbls TablePatch) where
  type Cov (QueryResultPatch tbls TablePatch) = QueryResultPatch tbls Proxy

instance DZippable tbls => Coverable (QueryResultPatch tbls TablePatch) where
  covered (QueryResultPatch xs) = QueryResultPatch $ runIdentity $ dmap (\(ComposeMaybe x) -> Identity $ ComposeMaybe $ Proxy <$ x) xs
  restrictCoverage (QueryResultPatch xs) (QueryResultPatch ys) =
    let (something, results) = dzip (\(ComposeMaybe x) (ComposeMaybe y) ->
          let z = liftA2 (\Proxy -> id) x y
          in (() <$ z, ComposeMaybe z)) xs ys
    in QueryResultPatch results <$ something

instance (DPointed tbls, Eq (tbls (ComposeMaybe Proxy))) => Coverage (QueryResultPatch tbls Proxy) where
  type WithFullCoverage (QueryResultPatch tbls Proxy) = QueryResultPatch tbls Proxy
  intersectionCoverage (QueryResultPatch xs) (QueryResultPatch ys) =
    let (something, results) = dzip (\(ComposeMaybe x) (ComposeMaybe y) ->
          let z = intersectionMaybeCoverage x y
          in (() <$ z, ComposeMaybe z)) xs ys
    in QueryResultPatch results <$ something
  differenceCoverage (QueryResultPatch xs) (QueryResultPatch ys) =
    let (something, results) = dzip (\(ComposeMaybe x) (ComposeMaybe y) ->
          let z = differenceMaybeCoverage x y
          in (() <$ z, ComposeMaybe z)) xs ys
    in QueryResultPatch results <$ something
  xorCoverage (QueryResultPatch xs) (QueryResultPatch ys) =
    let (something, results) = dzip (\(ComposeMaybe x) (ComposeMaybe y) ->
          let z = xorMaybeCoverage x y
          in (() <$ z, ComposeMaybe z)) xs ys
    in QueryResultPatch results <$ something

  unionCoverage (QueryResultPatch xs) (QueryResultPatch ys) =
    let Identity results = dzip (\(ComposeMaybe x) (ComposeMaybe y) ->
          let z = unionCoverage x y
          in Identity (ComposeMaybe z)) xs ys
    in QueryResultPatch results

instance (DPointed tbls, Eq (tbls (ComposeMaybe Proxy))) => FullCoverage (QueryResultPatch tbls Proxy) where
  fullCoverage = QueryResultPatch $ runIdentity $ dpure $ Identity $ ComposeMaybe $ Just Proxy

