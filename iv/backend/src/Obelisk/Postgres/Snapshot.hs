-- | Types and functions for manipulating PostgreSQL snapshots
--
-- See @txid_snapshot@ and related functions at <https://www.postgresql.org/docs/11/functions-info.html>
module Obelisk.Postgres.Snapshot
  ( TxidSnapshot (..)
  , txidSnapshotParser
  , transactionVisibleInSnapshot
  , transactionsBetweenSnapshots
  , unionTxidSnapshot
  , insertTxidSnapshot
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

-- | This is a type from Postgres that represents the committed transactions
-- that are visible to a given transaction.  We think of this type as a set
-- containing all those visible transactions.  Since the number of visible
-- transactions can be huge (i.e. every transaction that was ever committed to
-- the database), we don't represent each of them individually, but instead use
-- Postgres's representation, which effectively compresses away all the really
-- old ones.
data TxidSnapshot = TxidSnapshot
  { _txidSnapshot_xmin :: !Xid -- ^ All xids less than (but not equal to) this xid are visible
  , _txidSnapshot_xmax :: !Xid -- ^ Xids greater than or equal to this one are not visible
  , _txidSnapshot_xips :: !(Set Xid) -- ^ These xids are NOT visible; all other xids < xmax and >= xmin ARE visible
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

-- | Construct a TxidSnapshot whose set of visible transactions are the union of the two provided
unionTxidSnapshot :: TxidSnapshot -> TxidSnapshot -> TxidSnapshot
unionTxidSnapshot a b =
  let newXips = Set.union
        (Set.filter (\xip -> not $ transactionVisibleInSnapshot xip b) (_txidSnapshot_xips a))
        (Set.filter (\xip -> not $ transactionVisibleInSnapshot xip a) (_txidSnapshot_xips b))
      newXmax = max (_txidSnapshot_xmax a) (_txidSnapshot_xmax b)
  in TxidSnapshot
     { _txidSnapshot_xmin = computeXmin newXmax newXips
     , _txidSnapshot_xmax = newXmax
     , _txidSnapshot_xips = newXips
     }

computeXmin
  :: Xid -- ^ xmax
  -> Set Xid -- ^ xips
  -> Xid
computeXmin xmax xips = case Set.minView xips of
  Nothing -> xmax
  Just (minXip, _) -> minXip

-- | Add a visible transaction to a TxidSnapshot
insertTxidSnapshot :: Xid -> TxidSnapshot -> TxidSnapshot
insertTxidSnapshot xid old =
  let oldXmax = _txidSnapshot_xmax old
      -- All xids >= xmax are invisible, so to make this xid visible, xmax needs
      -- to be greater than this xid.  However, when we move xmax forward, we
      -- need to exclude any xids other than this xid by adding them to xips
      (newXmax, extraXips) = if xid >= oldXmax
        then (succ xid, Set.fromList [oldXmax .. pred xid])
        else (oldXmax, Set.empty)
      newXips = Set.delete xid (_txidSnapshot_xips old) `Set.union` extraXips
  in TxidSnapshot
     { _txidSnapshot_xmin = computeXmin newXmax newXips
     , _txidSnapshot_xmax = newXmax
     , _txidSnapshot_xips = newXips
     }
