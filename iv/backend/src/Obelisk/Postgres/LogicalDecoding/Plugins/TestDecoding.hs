{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.Postgres.LogicalDecoding.Plugins.TestDecoding
  ( line
  , xid
  , xidToBuilder
  , Transaction
  , Change (..)
  , Literal (..)
  , LineError (..)
  , TypeName (..)
  , Xid (..)
  , Message (..)
  , linesToTransactions
  ) where

import Prelude hiding (takeWhile)

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString (word8, notWord8)
import Data.Attoparsec.ByteString.Char8 as Attoparsec
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder, word32Dec)
import Data.Char (ord)
import qualified Data.Char as Char
import Data.Word
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Sequence (Seq, (|>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Database.PostgreSQL.Simple.Types
import qualified Data.Map as Map
import Data.Map (Map)
import Data.IORef
import Data.Maybe

--------------------------------------------------------------------------------
-- Potentially upstreamable
--------------------------------------------------------------------------------

stringWithSingleQuote :: Parser ByteString
stringWithSingleQuote = stringWithQuote $ fromIntegral $ ord '\''

stringWithDoubleQuote :: Parser ByteString
stringWithDoubleQuote = stringWithQuote $ fromIntegral $ ord '"'

stringWithQuote :: Word8 -> Parser ByteString
stringWithQuote q = do
  _ <- word8 q
  s <- many $ notWord8 q <|> doubledWord8 q
  _ <- word8 q
  pure $ BS.pack s

doubledWord8 :: Word8 -> Parser Word8
doubledWord8 c = do
  _ <- word8 c
  _ <- word8 c
  pure c

identifierLike :: (Char -> Bool) -> (Char -> Bool) -> Parser Text
identifierLike validFirstChar validOtherChar = fmap decodeUtf8 stringWithDoubleQuote <|> do
  -- Avoid plucking the first character off, so that we don't have to cons it back on (which would reallocate the ByteString)
  guard . validFirstChar =<< peekChar'
  decodeUtf8 <$> takeWhile1 validOtherChar

identifier :: Parser Identifier
identifier = Identifier <$> identifierLike identifierFirstChar identifierOtherChar

identifierFirstChar :: Char -> Bool
identifierFirstChar c = Char.isLetter c || c == '_'

identifierOtherChar :: Char -> Bool
identifierOtherChar c = identifierFirstChar c || isDigit c || c == '$'

qualifiedIdentifier :: Parser QualifiedIdentifier
qualifiedIdentifier = do
  namespace <- identifier
  _ <- char '.'
  name <- identifier
  pure $ QualifiedIdentifier (Just $ fromIdentifier namespace) $ fromIdentifier name

newtype TypeName = TypeName { unTypeName :: Text } deriving (Show, Read, Eq, Ord)

typeName :: Parser TypeName
typeName = do
  let otherChar c = identifierOtherChar c || c `elem` (" ()," :: String)
  TypeName <$> do
    tName <- identifierLike identifierFirstChar otherChar
    tDims <- many $ do
      _ <- char '['
      size <- option Nothing $ Just <$> decimal @Word
      _ <- char ']'
      pure size
    pure $ T.concat $ tName : ["[" <> maybe "" (T.pack . show) sizeM <> "]" | sizeM <- tDims]

newtype Xid = Xid { unXid :: Word32 } deriving (Show, Read, Eq, Ord, Enum)

xid :: Parser Xid
xid = Xid <$> decimal

xidToBuilder :: Xid -> Builder
xidToBuilder = word32Dec . unXid

--------------------------------------------------------------------------------

rowEntry :: Parser a -> Parser (Identifier, (TypeName, a))
rowEntry item = do
  colName <- identifier
  _ <- char '['
  colType <- typeName
  _ <- string "]:"
  v <- item
  pure (colName, (colType, v))

rowLike :: Parser a -> Parser (HashMap Identifier (TypeName, a))
rowLike item = {- (mempty <$ string "(no-tuple-data)") <|> -} do
  fmap HashMap.fromList $ flip sepBy1 (char ' ') $ rowEntry item

type Row = HashMap Identifier (TypeName, Maybe Literal)

data Literal
   = Literal_UnchangedToastDatum
   | Literal_Present ByteString
   deriving (Show, Read, Eq, Ord)

nullableLiteral :: Parser (Maybe Literal)
nullableLiteral = Nothing <$ string "null" <|> Just <$> literal

literal :: Parser Literal
literal = unchangedToastDatum <|> presentDatum
  where
    unchangedToastDatum = Literal_UnchangedToastDatum <$ string "unchanged-toast-datum"
    -- | We convert all present datums to their normal Postgres ByteString
    -- encoding, so that we don't have to preserve special cases throughout our
    -- code
    presentDatum :: Parser Literal
    presentDatum = Literal_Present <$> foldr1 (<|>)
      [ stringWithSingleQuote
      , char 'B' *> stringWithSingleQuote
      , "t" <$ string "true"
      , "f" <$ string "false"
      -- We assume anything else must be numeric
      , takeWhile1 (/= ' ')
      ]

row :: Parser Row
row = rowLike nullableLiteral

type Key = HashMap Identifier (TypeName, Literal) -- Keys can't contain nulls

key :: Parser Key
key = rowLike literal

data Change
   = Change_Insert Row
   | Change_Update (Maybe Key) Row
   | Change_Delete Key
   deriving (Show, Read, Eq, Ord)

change :: Parser Change
change = do
  t <- takeWhile (/= ':')
  _ <- string ": "
  case t of
    "INSERT" -> Change_Insert <$> row
    "UPDATE" -> foldr1 (<|>)
      [ do _ <- string "old-key: "
           old <- key
           _ <- string " new-tuple: "
           new <- row
           pure $ Change_Update (Just old) new
      , Change_Update Nothing <$> row
      ]
    "DELETE" -> Change_Delete <$> key
    _ -> fail $ "Unrecognized change type: " <> show t

data Line
   = Line_Begin Xid
   | Line_Change QualifiedIdentifier Change
   | Line_Commit Xid
   | Line_Message Message
   deriving (Show, Read, Eq, Ord)

data Message = Message
  { _message_transactional :: Bool --TODO: Deliver transactional messages with the transaction
  , _message_prefix :: ByteString
  , _message_content :: ByteString
  }
   deriving (Show, Read, Eq, Ord)

line :: Parser Line
line = do
  leader <- takeWhile (/= ' ')
  _ <- char ' '
  case leader of
    "BEGIN" -> Line_Begin <$> xid
    "table" -> do
      table <- qualifiedIdentifier
      _ <- string ": "
      Line_Change table <$> change
    "COMMIT" -> Line_Commit <$> xid
    "message:" -> do
      _ <- string "transactional: "
      transactional <- satisfy (inClass "01") >>= \case
        '0' -> pure False
        '1' -> pure True
        c -> fail $ "Unexpected character " <> show c
      _ <- string " prefix: "
      prefix <- takeWhile (/= ',') --NOTE: This is a limitation.  Technically, there can be commas in this string; however, if there are, the parse can potentially be ambiguous
      _ <- string ", sz: "
      sz <- decimal
      _ <- string " content:" -- There's no space after this one
      content <- Attoparsec.take sz
      pure $ Line_Message $ Message transactional prefix content
    _ -> fail $ "Unrecognized line leader: " <> show leader

type Transaction = (Xid, Map QualifiedIdentifier (Seq Change))

data LineError
   = LineError_OtherTransactionAlreadyInProgress
     Xid -- Existing transaction ID
     Xid -- New transaction ID that tried to start
   | LineError_CommittingWithoutTransaction
     Xid -- Transaction ID that tried to commit
   | LineError_CommittingWrongTransaction
     Xid -- Existing transaction ID
     Xid -- New transaction ID that tried to start
   | LineError_ChangeOutsideTransaction
  deriving Show

-- | Create a function that will statefully accumulate lines until a Transaction
-- is produced.  If the given line is not valid in the current state, an error
-- will be produced and the line will be ignored.  This should never happen for
-- a valid LogicalDecoding session, so all errors should be treated as serious.
linesToTransactions :: IO (Line -> IO (Either LineError (Maybe (Either Message Transaction))))
linesToTransactions = do
  pending <- newIORef Nothing
  return $ \l -> do
    let -- Update the pending transaction and possibly emit a message
        update :: Maybe Transaction -> Either LineError (Maybe Transaction, Maybe (Either Message Transaction))
        update = case l of
          Line_Begin newXid -> \case
            Nothing -> Right (Just (newXid, mempty), Nothing)
            Just (existingXid, _) -> Left $ LineError_OtherTransactionAlreadyInProgress existingXid newXid
          Line_Commit newXid -> \case
            Just txn@(existingXid, _)
              | existingXid == newXid -> Right (Nothing, Just $ Right txn)
              | otherwise -> Left $ LineError_CommittingWrongTransaction existingXid newXid
            Nothing -> Left $ LineError_CommittingWithoutTransaction newXid
          Line_Change qid c -> \old -> case old of
            Just (existingXid, changes) -> Right (Just (existingXid, Map.alter (Just . (|> c) . fromMaybe mempty) qid changes), Nothing)
            Nothing -> Left LineError_ChangeOutsideTransaction
          Line_Message msg -> \old -> Right (old, Just $ Left msg)
    atomicModifyIORef pending $ \old -> case update old of
      Left e -> (old, Left e)
      Right (new, a) -> (new, Right a)
