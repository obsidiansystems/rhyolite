{-|
Description:
  Create or modify accounts in the database

  This module does not handle notifications. See
  Rhyolite.Backend.Account.Notify for that
-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleContexts #-}
{-# Language MonoLocalBinds #-}
{-# Language OverloadedStrings #-}
module Rhyolite.Backend.Account.Db
  ( createAccount
  , login
  , ensureAccountExists
  , setAccountPassword
  , setAccountPasswordHash
  , makePasswordHash
  , passwordResetToken
  , newNonce
  , resetPassword
  , resetPasswordHash
  ) where

import Control.Monad (guard)
import Control.Monad.Trans.Maybe
import Crypto.PasswordStore
import Data.ByteString
import Data.Functor.Identity
import Data.Maybe
import Data.Signed
import Data.Signed.ClientSession
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Encoding as T
import Data.Time
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres
import Database.Beam.Postgres.Full hiding (insert)
import Database.PostgreSQL.Simple.Beam ()
import Rhyolite.Account
import Rhyolite.DB.Beam (current_timestamp_)
import Web.ClientSession as CS

-- | Creates a new account and emits a db notification about it
createAccount
  :: DatabaseEntity Postgres db (TableEntity Account)
  -> Text
  -> Text
  -> Pg (Either Text (PrimaryKey Account Identity))
createAccount accountTable email pass = do
  hash <- makePasswordHash pass
  accountIds <- runPgInsertReturningList $ flip returning _account_id $ insert accountTable $ insertExpressions
    [ Account
        { _account_id = default_
        , _account_email = lower_ (val_ email)
        , _account_password = val_ (Just hash)
        , _account_passwordResetNonce = just_ current_timestamp_
        }
    ]
  case accountIds of
    [accountId] -> pure $ Right $ AccountId accountId
    _ -> pure $ Left "Failed to create account"

-- | Attempts to login a user given some credentials.
login
  :: Database Postgres db
  => DatabaseEntity Postgres db (TableEntity Account)
  -> Text
  -> Text
  -> Pg (Maybe (PrimaryKey Account Identity))
login accountTable email pass = runMaybeT $ do
  (aid, mPwHash) <- MaybeT $ fmap listToMaybe $ runSelectReturningList $ select $ do
    acc <- all_ accountTable
    guard_ $ lower_ (_account_email acc) ==. lower_ (val_ email)
    pure (_account_id acc, _account_password acc)
  pwHash <- MaybeT $ pure mPwHash
  guard $ verifyPasswordWith pbkdf2 (2^) (T.encodeUtf8 pass) pwHash
  pure (AccountId aid)

ensureAccountExists
  :: (Database Postgres db)
  => DatabaseEntity Postgres db (TableEntity Account)
  -> Text
  -> Pg (Bool, PrimaryKey Account Identity)
ensureAccountExists accountTable email = do
  existingAccountId <- runSelectReturningOne $ select $ fmap primaryKey $ filter_ (\x ->
    lower_ (_account_email x) ==. lower_ (val_ email)) $ all_ accountTable
  case existingAccountId of
    Just existing -> return (False, existing)
    Nothing -> do
      results <- runInsertReturningList $ insert accountTable $ insertExpressions
        [ Account
            { _account_id = default_
            , _account_email = lower_ (val_ email)
            , _account_password = nothing_
            , _account_passwordResetNonce = nothing_
            }
        ]
      case results of
        [acc] -> do
          let aid = primaryKey acc
          pure (True, aid)
        _ -> error "ensureAccountExists: Creating account failed"

setAccountPassword
  :: DatabaseEntity Postgres db (TableEntity Account)
  -> PrimaryKey Account Identity
  -> Text
  -> Pg ()
setAccountPassword tbl aid password = do
  pw <- liftIO $ makePasswordHash password
  setAccountPasswordHash tbl aid pw

setAccountPasswordHash
  :: DatabaseEntity Postgres db (TableEntity Account)
  -> PrimaryKey Account Identity
  -> ByteString
  -> Pg ()
setAccountPasswordHash accountTable aid hash = runUpdate $ update accountTable
  (\x -> mconcat
    [ _account_password x <-. val_ (Just hash)
    , _account_passwordResetNonce x <-. nothing_
    ]
  )
  (\x -> primaryKey x ==. val_ aid)

makePasswordHash
  :: MonadIO m
  => Text
  -> m ByteString
makePasswordHash pw = do
  salt <- liftIO genSaltIO
  return $ makePasswordSaltWith pbkdf2 (2^) (encodeUtf8 pw) salt 14

resetPassword
  :: (Database Postgres db)
  => DatabaseEntity Postgres db (TableEntity Account)
  -> PrimaryKey Account Identity
  -> UTCTime
  -> Text
  -> Pg (Maybe (PrimaryKey Account Identity))
resetPassword tbl aid t pw = do
  hash <- makePasswordHash pw
  resetPasswordHash tbl aid t hash

resetPasswordHash
  :: (Database Postgres db)
  => DatabaseEntity Postgres db (TableEntity Account)
  -> PrimaryKey Account Identity
  -> UTCTime
  -> ByteString
  -> Pg (Maybe (PrimaryKey Account Identity))
resetPasswordHash accountTable aid nonce pwhash = do
  macc <- runSelectReturningOne $ lookup_ accountTable aid
  case macc of
    Nothing -> return Nothing
    Just a -> if _account_passwordResetNonce a == Just nonce
      then do
        setAccountPasswordHash accountTable aid pwhash
        return $ Just aid
      else fail "nonce mismatch"

passwordResetToken
  :: MonadIO m
  => CS.Key
  -> PrimaryKey Account Identity
  -> UTCTime
  -> m (Signed PasswordResetToken)
passwordResetToken csk aid nonce = do
  liftIO $ signWithKey csk $ PasswordResetToken (aid, nonce)

newNonce
  :: DatabaseEntity Postgres db (TableEntity Account)
  -> PrimaryKey Account Identity
  -> Pg (Maybe UTCTime)
newNonce accountTable aid = do
  a <- runUpdateReturningList $ update accountTable
    (\x -> _account_passwordResetNonce x <-. just_ current_timestamp_)
    (\x -> primaryKey x ==. val_ aid)
  pure $ case a of
    [acc] -> _account_passwordResetNonce acc
    _ -> Nothing
