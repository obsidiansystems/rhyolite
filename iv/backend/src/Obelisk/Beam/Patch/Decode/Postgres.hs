{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} --TODO: Eliminate this
module Obelisk.Beam.Patch.Decode.Postgres where

import Obelisk.Beam.Patch.Decoder

import Obelisk.Beam.DZippable
import Obelisk.Beam.Constraints
import Obelisk.Beam.Traversals
import Obelisk.Beam.TablesOnly
import Obelisk.Beam.TablesV
import Obelisk.Beam.Patch.Row
import Obelisk.Beam.Patch.Table
import Data.Sequence (Seq)
import Control.Lens
import Data.Foldable
import GHC.Generics hiding (R, C)
import GHC.Types
import Database.Beam
import Database.Beam.Backend.SQL.Types
import Database.Beam.Schema.Tables
import Database.PostgreSQL.Simple.Types
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (..), runConversion)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Database.PostgreSQL.Simple.Ok
import Database.PostgreSQL.Simple.Internal
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as TI
import qualified Database.PostgreSQL.LibPQ as PQ
import Obelisk.Postgres.LogicalDecoding.Plugins.TestDecoding
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Functor.Compose
import Witherable (catMaybes)

import Data.Patch.MapWithPatchingMove

deriving instance FromField a => FromField (SqlSerial a)

staticTypes :: [TI.TypeInfo]
staticTypes = catMaybes $ TI.staticTypeInfo <$> [Oid i | i <- [0..65535]]

staticTypesByName :: Map Text TI.TypeInfo
staticTypesByName = Map.fromList $ [(decodeUtf8 $ TI.typname t, t) | t <- staticTypes]

lookupStaticTypeByName :: Text -> Maybe TI.TypeInfo
lookupStaticTypeByName typeName = asum
  [ Map.lookup typeName staticTypesByName
  , -- aliases
    case typeName of
      "bigint" -> Just TI.int8
      "smallint" -> Just TI.int2
      "integer" -> Just TI.int4
      "character varying" -> Just TI.text
      "boolean" -> Just TI.bool
      "timestamp with time zone" -> Just TI.timestamptz
      "timestamp without time zone" -> Just TI.timestamp
      "double precision" -> Just TI.float8
      _ -> Nothing
  , -- arrays
    case T.breakOn "[" typeName of
      (_, "") -> Nothing
      (tName, _) -> asum
        [ -- postgres prefixes array types with '_'
          Map.lookup ("_" <> tName) staticTypesByName
        , do
            t <- lookupStaticTypeByName tName
            flip find staticTypes $ \case
              TI.Array _ _ _ _ telem -> TI.typoid telem == TI.typoid t
              _ -> False
        ]
  ]

decodeRow
  :: forall tbl tbl'
  .  Beamable tbl
  => Connection
  -> tbl (TableField tbl')
  -> tbl (FieldDecoder Postgres)
  -> HashMap Identifier (TypeName, Maybe Literal)
  -> IO (tbl Maybe)
decodeRow conn settings decoders r = zipBeamFieldsM d settings decoders
  where d :: forall a. Columnar' (TableField tbl') a -> Columnar' (FieldDecoder Postgres) a -> IO (Columnar' Maybe a)
        d (Columnar' fieldSettings) (Columnar' FieldDecoder) = do
          let n = _fieldName fieldSettings
          result <- case HashMap.lookup (Identifier n) r of
            Just (TypeName typeName, v) -> do
              let --TODO: Retrieve types properly; do we need to cache them?
                  fieldType = fromMaybe (error $ "Unrecognized type: " <> show typeName) $ lookupStaticTypeByName typeName
                  f = Field
                    { typeOid = TI.typoid fieldType
                    , format = PQ.Text --TODO: Is this right?
                    , tableOid = Nothing
                    , name = Just $ encodeUtf8 n
                    }
              case v of
                Just (Literal_Present l) -> Just <$> runConversion (fromField f $ Just l) conn
                Nothing -> Just <$> runConversion (fromField f Nothing) conn
                Just Literal_UnchangedToastDatum -> pure Nothing
            Nothing -> pure Nothing
          case result of
            Just (Ok a) -> pure $ Columnar' $ Just a
            Just (Errors e) -> fail $ show e
            Nothing -> pure $ Columnar' Nothing

decodeChange
  :: forall tbl
  .  ( Table tbl
     , Ord (PrimaryKey tbl Identity)
     )
  => Connection
  -> tbl (TableField tbl)
  -> tbl (FieldDecoder Postgres)
  -> Change
  -> IO (PatchMapWithPatchingMove (PrimaryKey tbl Identity) (RowPatch tbl))
decodeChange conn settings decoders =
  let decodeRecord = decodeRow conn settings decoders
      decodeKey k = do
        v <- decodeRow conn (primaryKey settings) (primaryKey decoders) (fmap (fmap Just) k)
        case sequenceMaybeBeamable v of
          Nothing -> fail "Got partial key"
          Just a -> pure a
  in \case
  Change_Insert rRaw -> do
    rm <- decodeRecord rRaw
    case sequenceMaybeBeamable rm of
      Nothing -> fail "Insert got partial row"
      Just r -> pure $ patchMapWithPatchingMoveInsertAll $ Map.singleton (primaryKey r) r
  Change_Update mkRaw rRaw -> do
    mk <- traverse decodeKey mkRaw
    vm <- decodeRecord rRaw
    case sequenceMaybeBeamable (primaryKey vm) of
      Nothing -> fail "Update without key"
      Just newKey -> do
        case mk of
          Just oldKey
            | oldKey /= newKey -> do
                pure $ PatchMapWithPatchingMove $ Map.fromList --TODO: Factor out this patch creation into Data.Patch.MapWithPatchingMove
                  [ (newKey, NodeInfo
                      { _nodeInfo_from = From_Move oldKey $ RowPatch vm
                      , _nodeInfo_to = Nothing
                      }
                    )
                  , (oldKey, NodeInfo
                      { _nodeInfo_from = From_Delete
                      , _nodeInfo_to = Just newKey
                      }
                    )
                  ]
          _ -> pure $ PatchMapWithPatchingMove $ Map.singleton newKey $ NodeInfo
            { _nodeInfo_from = From_Move newKey $ RowPatch vm
            , _nodeInfo_to = Just newKey
            }
  Change_Delete kRaw -> do
    k <- decodeKey kRaw
    pure $ PatchMapWithPatchingMove $ Map.singleton k $ NodeInfo
      { _nodeInfo_from = From_Delete
      , _nodeInfo_to = Nothing
      }

type DecodableDatabase db =
  ( Database Postgres db
  , Generic (db (EntityPatchDecoder Postgres))
  , GPatchDatabase (Rep (db (EntityPatchDecoder Postgres)) ())
  )

decodeTransaction
  :: forall db
  .  ( DecodableDatabase db
     , HasT IsTable db
     , DZippable db
     )
  => DatabaseSettings Postgres db
  -> Connection
  -> Map QualifiedIdentifier (Seq Change)
  -> IO (TablesV db TablePatch)
decodeTransaction db conn tableChanges = TablesV <$> zipTablesOnly f (toTablesOnly db) (toTablesOnly $ defaultDatabasePatchDecoder @Postgres @db)
  where
    f :: forall tbl. Compose (DatabaseEntity Postgres db) TableEntity tbl -> Compose (EntityPatchDecoder Postgres) TableEntity tbl -> IO (TablePatch tbl)
    f (Compose (DatabaseEntity descriptor)) (Compose decoder) =
      let n = view dbEntityName descriptor
          s = Just "public" -- view dbEntitySchema descriptor --TODO: Beam has 'Nothing' while the replication connection reports 'Just "public"'
          vals = Map.findWithDefault mempty (QualifiedIdentifier s n) tableChanges
          x = case decoder of
                EntityPatchDecoder_NotDecodable -> error "impossible"
                EntityPatchDecoder_Table decoders -> do
                  TablePatch . mconcat . reverse . toList <$> traverse (decodeChange conn (dbTableSettings descriptor) decoders) vals
                  -- THIS REVERSE        ^^^^^^^
                  -- is neccessary because Patch instances are applied with newer patches coming from the left; so if we're folding things together, we need the newest thing on the left; but decoded will have them oldest elements first.
      in x
