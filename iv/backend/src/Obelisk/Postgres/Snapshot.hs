-- | Types and functions for manipulating PostgreSQL snapshots
--
-- See @txid_snapshot@ and related functions at <https://www.postgresql.org/docs/11/functions-info.html>
module Obelisk.Postgres.Snapshot
  ( TxidSnapshot (..)
  , txidSnapshotParser
  , transactionVisibleInSnapshot
  , transactionsBetweenSnapshots
  ) where

import Control.Monad
import Control.Applicative
import Control.Exception
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder
import Data.Set (Set)
import qualified Data.Set as Set
import Database.PostgreSQL.Simple.FromField (FromField (..), typename)
import Database.PostgreSQL.Simple.ToField (ToField (..), Action (..))
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Obelisk.Postgres.LogicalDecoding.Plugins.TestDecoding (Xid (..), xidToBuilder)
import qualified Obelisk.Postgres.LogicalDecoding.Plugins.TestDecoding as TestDecoding
import Data.List

data TxidSnapshot = TxidSnapshot
  { _txidSnapshot_xmin :: !Xid
  , _txidSnapshot_xmax :: !Xid
  , _txidSnapshot_xips :: !(Set Xid)
  }
  deriving (Show, Read, Eq, Ord)

instance FromField TxidSnapshot where
  fromField f = \case
    Nothing -> fail "TxidSnapshot cannot be null"
    Just d -> do
      typ <- typename f -- Is this check worth it?
      when (typ /= "txid_snapshot") $ fail $ "TxidSnapshot has wrong postgres type: " <> show typ
      case A.parseOnly (txidSnapshotParser <* A.endOfInput) d of
        Right s -> pure s
        Left l -> fail $ show l

instance ToField TxidSnapshot where
  toField = Escape . LBS.toStrict . toLazyByteString . txidSnapshotToBuilder

txidSnapshotParser :: Parser TxidSnapshot
txidSnapshotParser = do
  xmin <- TestDecoding.xid
  _ <- A.char ':'
  xmax <- TestDecoding.xid
  _ <- A.char ':'
  xips <- ([] <$ A.endOfInput) <|> A.sepBy1 TestDecoding.xid (A.char ',')
  pure $ TxidSnapshot xmin xmax $ Set.fromList xips

txidSnapshotToBuilder :: TxidSnapshot -> Builder
txidSnapshotToBuilder s = mconcat
  [ xidToBuilder $ _txidSnapshot_xmin s
  , byteString ":"
  , xidToBuilder $ _txidSnapshot_xmax s
  , byteString ":"
  , mconcat $ intersperse (byteString ",") $ fmap xidToBuilder $ Set.toList $ _txidSnapshot_xips s
  ]

-- | A transaction is "before" the read if its xid is < xmin or if it is < xmax and not present in xips
transactionVisibleInSnapshot :: Xid -> TxidSnapshot -> Bool
transactionVisibleInSnapshot xid s = xid < _txidSnapshot_xmin s || (xid < _txidSnapshot_xmax s && not (xid `Set.member` _txidSnapshot_xips s))

-- | List all the 'Xid's that are logically between the first and second snapshot
-- The second snapshot must be strictly LATER than the first
transactionsBetweenSnapshots :: TxidSnapshot -> TxidSnapshot -> Set Xid
transactionsBetweenSnapshots s1 s2 = assert ok result
  where previouslyHiddenNowVisible = _txidSnapshot_xips s1 `Set.difference` _txidSnapshot_xips s2
        previouslyInFutureNowInPresent = Set.fromList [_txidSnapshot_xmax s1 .. pred (_txidSnapshot_xmax s2)]
        previouslyInFutureNowVisible = previouslyInFutureNowInPresent `Set.difference` _txidSnapshot_xips s2
        result = previouslyHiddenNowVisible `Set.union` previouslyInFutureNowVisible
        ok = and
          [ _txidSnapshot_xmin s1 <= _txidSnapshot_xmin s2
          , _txidSnapshot_xmax s1 <= _txidSnapshot_xmax s2
          , all (not . (`transactionVisibleInSnapshot` s1)) $ _txidSnapshot_xips s2 -- Anything in progress in s2 can't be visible in s1
          --TODO: Are there more requirements?
          ]
