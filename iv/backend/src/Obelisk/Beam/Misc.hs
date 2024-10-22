{-# LANGUAGE QuantifiedConstraints #-}
module Obelisk.Beam.Misc
  ( replaceAllTableContents
  , ValType (..)
  , coerceQExprResult
  , coerceDataType
  ) where

import Control.Lens ((^.), (.~))
import Control.Monad
import Data.Function
import Data.Functor.Identity
import Data.List.Split (chunksOf)
import Data.Proxy
import Data.Time.Calendar
import Database.Beam as Beam
import Database.Beam.Backend.SQL
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres
import qualified Database.Beam.Postgres.Full as Pg
import Database.Beam.Postgres.Syntax
import Database.Beam.Query.Internal
import Database.Beam.Schema.Tables
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Types
import GHC.Generics
import Obelisk.Api
import Obelisk.Beam

-- | Delete everything from the given table and replace it with the given list of items, being careful not to create more churn than necessary.  In particular: for items that haven't changed, a new row will not be written to the WAL or sent to replication clients.
replaceAllTableContents
  :: forall db tbl
  .  ( Generic (tbl (HasConstraint (HasSqlValueSyntax PgValueSyntax)))
     , Generic (tbl Identity)
     , Generic (tbl Exposed)
     , Generic (tbl (HasConstraint (HasSqlEqualityCheck Postgres)))
     , GFieldsFulfillConstraint
       (HasSqlValueSyntax PgValueSyntax)
       (Rep (tbl Exposed))
       (Rep (tbl (HasConstraint (HasSqlValueSyntax PgValueSyntax))))
     , GFieldsFulfillConstraint
       (HasSqlEqualityCheck Postgres)
       (Rep (tbl Exposed))
       (Rep (tbl (HasConstraint (HasSqlEqualityCheck Postgres))))
     , Generic (PrimaryKey tbl (HasConstraint (HasSqlValueSyntax PgValueSyntax)))
     , Generic (PrimaryKey tbl Identity)
     , Generic (PrimaryKey tbl Exposed)
     , GFieldsFulfillConstraint
       (HasSqlValueSyntax PgValueSyntax)
       (Rep (PrimaryKey tbl Exposed))
       (Rep (PrimaryKey tbl (HasConstraint (HasSqlValueSyntax PgValueSyntax))))
     , Table tbl
     , Ord (PrimaryKey tbl Identity) --TODO: Technically, we also need to ensure that the database and Haskell *agree* on the ordering
     , Show (PrimaryKey tbl Identity)
     , Database Postgres db
     )
  => DatabaseEntity Postgres db (TableEntity tbl)
  -> [tbl Identity]
  -> WriteDb ()
replaceAllTableContents tbl@(DatabaseEntity tblDescriptor) allVals = do
  --TODO: Don't include the primary key fields in the update: we never change them
  --TODO: Select chunk size more meaningfully OR rather than chunking, use COPY ... FROM STDIN to fill up a temporary table, then do everything on the DB side
  let tblIdentifier = QualifiedIdentifier (tblDescriptor ^. dbEntitySchema) (tblDescriptor ^. dbEntityName)
      tempTbl :: DatabaseEntity Postgres db (TableEntity tbl) --TODO: this phantom is a bit off - maybe it should be something representing the fact that this DB schema has been temporarily augmented
      tempTbl = DatabaseEntity $ tblDescriptor
        & dbEntitySchema .~ Just "pg_temp"
        & dbEntityName .~ "replacement_data"
  unsafeWriteDb $ \(conn, _) -> void $ execute conn [sql| CREATE TEMPORARY TABLE replacement_data (LIKE ?) |] $ Only tblIdentifier
  forM_ (zip [1..] $ chunksOf 1000 allVals) $ \(chunkNumber :: Int, chunk :: [tbl Identity]) -> do
    unsafeWriteDb $ \_ -> putStrLn $ "replaceAllTableContents: Inserting chunk " <> show chunkNumber <> " with keys " <> show (fmap primaryKey chunk) <> " into temp table"
    runInsert' $ insert tempTbl $ insertValues chunk
  unsafeWriteDb $ \_ -> putStrLn "replaceAllTableContents: Inserting temp table into main table"
  runInsert' $ Pg.insert tbl (insertFrom $ all_ tempTbl) $ Pg.onConflict (conflictingFields primaryKey) $ onConflictUpdateAllWhere $ \a b -> (fieldsToExprs a) /=. b
  unsafeWriteDb $ \_ -> putStrLn "replaceAllTableContents: Deleting unneeded values"
  runDelete' $ delete tbl $ \t -> not_ $ primaryKey t `in_'` subquery_ (fmap (QExpr . toProjectionExpr . primaryKey) $ all_ tempTbl)
  unsafeWriteDb $ \_ -> putStrLn "replaceAllTableContents: Done"
  unsafeWriteDb $ \(conn, _) -> void $ execute_ conn [sql| DROP TABLE pg_temp.replacement_data |]

in_'
  :: forall be context table s a
  .  ( IsSql92ExpressionSyntax
       (Sql92SelectTableExpressionSyntax
         (Sql92SelectSelectTableSyntax
           (Sql92SelectSyntax (BeamSqlBackendSyntax be))))
     , Beamable table
     )
  => table (QGenExpr context be s) -> QGenExpr context be s a -> QGenExpr QValueContext be s Bool
in_' row (QExpr options) = QExpr (inE <$> toRowExpr @be row <*> fmap (:[]) options)

toRowExpr
  :: forall be context table s
  . ( Beamable table
    , IsSql92ExpressionSyntax
      (Sql92SelectTableExpressionSyntax
        (Sql92SelectSelectTableSyntax
          (Sql92SelectSyntax (BeamSqlBackendSyntax be)))))
  => table (QGenExpr context be s)
  -> TablePrefix
  -> BeamSqlBackendExpressionSyntax be
toRowExpr = fmap rowE . sequence . allBeamValues (\(Columnar' (QExpr x)) -> x)

toProjectionExpr
  :: forall be context table s
  . ( Beamable table
    , IsSql92ExpressionSyntax
      (Sql92SelectTableExpressionSyntax
        (Sql92SelectSelectTableSyntax
          (Sql92SelectSyntax (BeamSqlBackendSyntax be))))
    , be ~ Postgres
    )
  => table (QGenExpr context be s)
  -> TablePrefix
  -> BeamSqlBackendExpressionSyntax be
toProjectionExpr = fmap (PgExpressionSyntax . fromPgProjection . projExprs . flip zip (repeat Nothing)) . sequence . allBeamValues (\(Columnar' (QExpr x)) -> x)

fieldsToExprs
  :: ( Beamable tbl
     , IsSql92ExpressionSyntax
       (Sql92SelectTableExpressionSyntax
         (Sql92SelectSelectTableSyntax
           (Sql92SelectSyntax
             (BeamSqlBackendSyntax be))))
     )
  => tbl (QField s)
  -> tbl (QExpr be s)
fieldsToExprs = changeBeamRep (\(Columnar' (QField _ t nm)) -> Columnar' (QExpr (pure (fieldE (qualifiedField t nm)))))

class (HasSqlValueSyntax PgValueSyntax a) => ValType a where
  typeOf_
    :: (a ~ HaskellLiteralForQExpr (QGenExpr ctxt be s a))
    => Proxy a
    -> DataType Postgres a
  valType_
    :: (a ~ HaskellLiteralForQExpr (QGenExpr ctxt be s a))
    => a
    -> QGenExpr ctxt Postgres s a
  valType_ d = cast_
    (val_ d)
    (typeOf_ (Proxy :: Proxy a))

instance (forall ctxt s. SqlJustable (QGenExpr ctxt Postgres s t) (QGenExpr ctxt Postgres s (Maybe t)), ValType t) => ValType (Maybe t) where
  typeOf_ _ = maybeType $ typeOf_ (Proxy @t)
  valType_ :: forall ctxt s. (Maybe t ~ HaskellLiteralForQExpr (QGenExpr ctxt Postgres s (Maybe t))) => Maybe t -> QGenExpr ctxt Postgres s (Maybe t)
  valType_ = \case
    Nothing -> nothing_ @(QGenExpr ctxt Postgres s t)
    Just v -> cast_
      (val_ v)
      (typeOf_ (Proxy @(Maybe t)))

instance ValType Day where
  typeOf_ Proxy = date

coerceQExprResult :: forall a b ctxt be s. QGenExpr ctxt be s a -> QGenExpr ctxt be s b
coerceQExprResult (QExpr e) = QExpr e

coerceDataType :: forall a b be. DataType be a -> DataType be b
coerceDataType (DataType e) = DataType e
