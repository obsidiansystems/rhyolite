{-|
Description:
  Check or modify credentials
-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleContexts #-}
{-# Language MonoLocalBinds #-}
{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}
{-# Language GADTs #-}
module Rhyolite.Backend.Account
  ( createAccount
  , login
  , ensureAccountExists
  , setAccountPassword
  , setAccountPasswordHash
  , makePasswordHash
  , passwordResetToken
  , newNonce
  , handleAccountRequest
  , AccountContext (..)
  ) where

import Control.Monad (guard)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Crypto.PasswordStore
import Data.Aeson
import Data.ByteString
import Data.Constraint.Extras
import Data.Constraint.Forall
import Data.Functor
import Data.Functor.Identity
import Data.Maybe
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Encoding as T
import Data.Time
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres
import Data.Signed
import Data.Signed.ClientSession
import Database.Beam.Postgres.Full hiding (insert)
import Database.Beam.Postgres.Syntax
import Database.PostgreSQL.Simple.Beam ()
import Rhyolite.Account
import Rhyolite.DB.Beam (current_timestamp_, genRandomUuid_)
import System.Entropy.Class
import Web.ClientSession as CS

data AccountContext db m = AccountContext
  { _accountContext_table :: DatabaseEntity Postgres db (TableEntity Account)
  , _accountContext_key :: CS.Key
  , _accountContext_sendFinishCreateAccount :: Email -> Signed PasswordResetToken -> m ()
  , _accountContext_sendResetPassword :: Email -> Signed PasswordResetToken -> m ()
  }

handleAccountRequest
  :: ( MonadBeam Postgres m
     , Database Postgres db
     , EntropyGenerator m
     , MonadFail m
     )
  => AccountContext db m
  -> AccountRequest a
  -> m a
handleAccountRequest ctx = \case
  AccountRequest_Login email password -> login ctx email password
  AccountRequest_CreateAccount email -> createAccount ctx email
  AccountRequest_FinishAccountCreation token password -> finishAccountCreation ctx token password
  AccountRequest_ForgotPassword email -> createAccount ctx email --TODO: Not createAccount
  AccountRequest_ResetPassword token password -> finishAccountCreation ctx token password --TODO: Not finishAccountCreation

-- FUTURE: Mitigate timing attacks, e.g. by verifying against a fake password
-- hash even if the record is not found, or by ensuring that all login requests
-- wait for the same amount of time.

-- | Attempts to login a user given some credentials.
login
  :: ( MonadBeam Postgres m
     , Database Postgres db
     , EntropyGenerator m
     )
  => AccountContext db m
  -> Email
  -> Password
  -> m (Maybe (Signed AuthToken))
login ctx email pass = runMaybeT $ do
  (aid, mPwHash) <- MaybeT $ fmap listToMaybe $ runSelectReturningList $ select $ do
    acc <- all_ $ _accountContext_table ctx
    guard_ $ lower_ (_account_email acc) ==. lower_ (val_ email)
    pure (_account_id acc, _account_password acc)
  pwHash <- MaybeT $ pure mPwHash
  guard $ verifyPasswordWith pbkdf2 (2^) (T.encodeUtf8 pass) pwHash
  lift $ signWithKey (_accountContext_key ctx) $ AuthToken $ AccountId aid

-- | Creates a new account
createAccount
  :: ( MonadBeam Postgres m
     , Database Postgres db
     , EntropyGenerator m
     , MonadFail m
     )
  => AccountContext db m
  -> Email
  -> m ()
createAccount ctx email = do
  accountIds <- runPgInsertReturningList $ flip returning (\a -> (pk a, _account_passwordResetNonce a)) $ insert (_accountContext_table ctx) $ insertExpressions
    [ Account
        { _account_id = genRandomUuid_
        , _account_email = lower_ (val_ email)
        , _account_password = nothing_
        , _account_passwordResetNonce = just_ current_timestamp_
        }
    ]
  --TODO: Send the email
  case accountIds of
    [(accountId, Just nonce)] -> do
      token <- signWithKey (_accountContext_key ctx) $ PasswordResetToken (accountId, nonce)
      _accountContext_sendFinishCreateAccount ctx email token
    _ -> fail "Failed to create account"

-- FUTURE: Expiration policy for password reset tokens
finishAccountCreation
  :: ( MonadBeam Postgres m
     , Database Postgres db
     , EntropyGenerator m
     , MonadFail m
     )
  => AccountContext db m
  -> Signed PasswordResetToken
  -> Password
  -> m (Either FinishAccountCreationError (Signed AuthToken))
finishAccountCreation ctx token password = case readSignedWithKey (_accountContext_key ctx) token of
  Nothing -> pure $ Left FinishAccountCreationError_InvalidToken
  Just (PasswordResetToken (accountId, nonceFromToken)) -> do
    nonces <- runSelectReturningList $ select $ do
      account <- all_ $ _accountContext_table ctx
      guard_ $ pk account ==. val_ accountId
      pure $ _account_passwordResetNonce account
    case nonces of
      [Just nonceFromDatabase]
        | nonceFromDatabase == nonceFromToken
          -> do
            setAccountPassword ctx accountId password
            fmap Right $ signWithKey (_accountContext_key ctx) $ AuthToken accountId
      _ -> pure $ Left FinishAccountCreationError_InvalidToken

ensureAccountExists
  :: ( MonadBeam Postgres m
     , Database Postgres db
     , EntropyGenerator m
     , MonadFail m
     , MonadBeamInsertReturning Postgres m
     )
  => AccountContext db m
  -> Email
  -> m (Bool, PrimaryKey Account Identity)
ensureAccountExists ctx email = do
  existingAccountId <- runSelectReturningOne $ select $ fmap primaryKey $ filter_ (\x ->
    lower_ (_account_email x) ==. lower_ (val_ email)) $ all_ (_accountContext_table ctx)
  case existingAccountId of
    Just existing -> return (False, existing)
    Nothing -> do
      -- FUTURE: Use ON CONFLICT
      results <- runInsertReturningList $ insert (_accountContext_table ctx) $ insertExpressions
        [ Account
            { _account_id = genRandomUuid_
            , _account_email = lower_ (val_ email)
            , _account_password = nothing_
            , _account_passwordResetNonce = nothing_
            }
        ]
      case results of
        [acc] -> do
          let aid = primaryKey acc
          pure (True, aid)
        _ -> fail "ensureAccountExists: Creating account failed"

setAccountPassword
  :: ( MonadBeam Postgres m
     , Database Postgres db
     , EntropyGenerator m
     , MonadFail m
     )
  => AccountContext db m
  -> PrimaryKey Account Identity
  -> Password
  -> m ()
setAccountPassword ctx aid password = do
  pw <- makePasswordHash password
  setAccountPasswordHash ctx aid pw

setAccountPasswordHash
  :: ( MonadBeam Postgres m
     , Database Postgres db
     , EntropyGenerator m
     , MonadFail m
     )
  => AccountContext db m
  -> PrimaryKey Account Identity
  -> ByteString
  -> m ()
setAccountPasswordHash ctx aid hash = runUpdate $ update (_accountContext_table ctx)
  (\x -> mconcat
    [ _account_password x <-. val_ (Just hash)
    , _account_passwordResetNonce x <-. nothing_
    ]
  )
  (\x -> primaryKey x ==. val_ aid)

makePasswordHash
  :: ( EntropyGenerator m
     , Functor m
     )
  => Text
  -> m ByteString
makePasswordHash pw = getEntropy 16 <&> \saltRaw ->
  makePasswordSaltWith pbkdf2 (2^) (encodeUtf8 pw) (makeSalt saltRaw) 14

passwordResetToken
  :: MonadIO m
  => CS.Key
  -> PrimaryKey Account Identity
  -> UTCTime
  -> m (Signed PasswordResetToken)
passwordResetToken csk aid nonce = do
  liftIO $ signWithKey csk $ PasswordResetToken (aid, nonce)

newNonce
  :: ( MonadBeam Postgres m
     , Database Postgres db
     , EntropyGenerator m
     , MonadFail m
     , MonadBeamUpdateReturning Postgres m
     )
  => AccountContext db m
  -> PrimaryKey Account Identity
  -> m (Maybe UTCTime)
newNonce ctx aid = do
  a <- runUpdateReturningList $ update (_accountContext_table ctx)
    (\x -> _account_passwordResetNonce x <-. just_ current_timestamp_)
    (\x -> primaryKey x ==. val_ aid)
  pure $ case a of
    [acc] -> _account_passwordResetNonce acc
    _ -> Nothing
