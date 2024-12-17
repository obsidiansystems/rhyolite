{-|
Description:
  Check or modify credentials
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wredundant-constraints -Werror=redundant-constraints #-}
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
  , AccountMessage (..)
  , AccountTable
  , AuthKey
  , SendAccountMessage
  , DbMonad
  , DbSchema
  ) where

import Control.Monad (guard)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Provide
import Crypto.PasswordStore
import Data.Aeson
import Data.ByteString
import Data.Constraint.Extras
import Data.Constraint.Forall
import Data.Functor
import Data.Functor.Identity
import Data.Maybe
import Data.Proxy
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
import Database.Beam.Postgres.Syntax
import Database.PostgreSQL.Simple.Beam ()
import Rhyolite.Account
import Rhyolite.DB.Beam (current_timestamp_, genRandomUuid_)
import System.Entropy.Class
import Web.ClientSession as CS

-- | The database schema in which the accounts table lives
type family DbSchema tctx :: (* -> *) -> *

-- | The monad in which database functions present in the provider run
type family DbMonad tctx :: * -> *

-- | The database table in which accounts are recorded
data AccountTable
type instance ValueType tctx AccountTable = DatabaseEntity Postgres (DbSchema tctx) (TableEntity Account)

-- | Provider key for the signing key that will be used to sign and verify
-- account tokens
data AuthKey
type instance ValueType tctx AuthKey = CS.Key

-- | A function that should send an AccountMessage; will be invoked in this
-- module when messages ought to be sent to the user for account management
-- purposes
data SendAccountMessage
type instance ValueType tctx SendAccountMessage = Email -> AccountMessage -> DbMonad tctx ()

data AccountMessage
   = AccountMessage_FinishAccountCreation (Signed PasswordResetToken)
   | AccountMessage_AccountAlreadyExists
   | AccountMessage_ResetPassword (Signed PasswordResetToken)
   | AccountMessage_AccountDoesNotExist

handleAccountRequest
  :: forall db m p a
  .  ( MonadBeam Postgres m
     , Database Postgres (DbSchema (ProviderTypeContext p))
     , EntropyGenerator m
     , MonadFail m
     , p `Provides` AccountTable
     , p `Provides` AuthKey
     , p `Provides` SendAccountMessage
     , m ~ DbMonad (ProviderTypeContext p)
     )
  => Proxy p
  -> AccountRequest a
  -> m a
handleAccountRequest p = \case
  AccountRequest_Login email password -> login p email password
  AccountRequest_CreateAccount email -> createAccount p email
  AccountRequest_FinishAccountCreation token password -> finishAccountCreation p token password
  AccountRequest_ForgotPassword email -> forgotPassword p email
  AccountRequest_ResetPassword token password -> finishAccountCreation p token password --TODO: Not finishAccountCreation

-- FUTURE: Mitigate timing attacks, e.g. by verifying against a fake password
-- hash even if the record is not found, or by ensuring that all login requests
-- wait for the same amount of time.

-- | Attempts to login a user given some credentials.
login
  :: forall m p
  .  ( MonadBeam Postgres m
     , Database Postgres (DbSchema (ProviderTypeContext p))
     , EntropyGenerator m
     , p `Provides` AccountTable
     , p `Provides` AuthKey
     )
  => Proxy p
  -> Email
  -> Password
  -> m (Maybe (Signed AuthToken))
login p email pass = runMaybeT $ do
  (aid, mPwHash) <- MaybeT $ fmap listToMaybe $ runSelectReturningList $ select $ do
    acc <- all_ $ providedP @AccountTable p
    guard_ $ lower_ (_account_email acc) ==. lower_ (val_ email)
    pure (_account_id acc, _account_password acc)
  pwHash <- MaybeT $ pure mPwHash
  guard $ verifyPasswordWith pbkdf2 (2^) (T.encodeUtf8 pass) pwHash
  lift $ signWithKey (providedP @AuthKey p) $ AuthToken $ AccountId aid

-- | Creates a new account
createAccount
  :: forall m p
  .  ( MonadBeam Postgres m
     , EntropyGenerator m
     , p `Provides` AccountTable
     , p `Provides` AuthKey
     , p `Provides` SendAccountMessage
     , m ~ DbMonad (ProviderTypeContext p)
     )
  => Proxy p
  -> Email
  -> m ()
createAccount p email = do
  --TODO: On conflict skip
  accountIds <- runPgInsertReturningList $ flip returning (\a -> (pk a, _account_passwordResetNonce a)) $ insert (providedP @AccountTable p) $ insertExpressions
    [ Account
        { _account_id = genRandomUuid_
        , _account_email = lower_ (val_ email)
        , _account_password = nothing_
        , _account_passwordResetNonce = just_ current_timestamp_
        }
    ]
  case accountIds of
    [(accountId, Just nonce)] -> do
      token <- signWithKey (providedP @AuthKey p) $ PasswordResetToken (accountId, nonce)
      providedP @SendAccountMessage p email $ AccountMessage_FinishAccountCreation token
    _ -> do
      providedP @SendAccountMessage p email AccountMessage_AccountAlreadyExists

-- FUTURE: Expiration policy for password reset tokens
finishAccountCreation
  :: forall m p
  .  ( MonadBeam Postgres m
     , Database Postgres (DbSchema (ProviderTypeContext p))
     , EntropyGenerator m
     , p `Provides` AccountTable
     , p `Provides` AuthKey
     )
  => Proxy p
  -> Signed PasswordResetToken
  -> Password
  -> m (Either FinishAccountCreationError (Signed AuthToken))
finishAccountCreation p token password = case readSignedWithKey (providedP @AuthKey p) token of
  Nothing -> pure $ Left FinishAccountCreationError_InvalidToken
  Just (PasswordResetToken (accountId, nonceFromToken)) -> do
    nonces <- runSelectReturningList $ select $ do
      account <- all_ $ providedP @AccountTable p
      guard_ $ pk account ==. val_ accountId
      pure $ _account_passwordResetNonce account
    case nonces of
      [Just nonceFromDatabase]
        | nonceFromDatabase == nonceFromToken
          -> do
            setAccountPassword p accountId password
            fmap Right $ signWithKey (providedP @AuthKey p) $ AuthToken accountId
      _ -> pure $ Left FinishAccountCreationError_InvalidToken

-- | Sends the user a password reset email, only if their account already exists
forgotPassword
  :: forall m p
  .  ( MonadBeam Postgres m
     , Database Postgres (DbSchema (ProviderTypeContext p))
     , EntropyGenerator m
     , MonadFail m
     , p `Provides` AccountTable
     , p `Provides` AuthKey
     , p `Provides` SendAccountMessage
     , m ~ DbMonad (ProviderTypeContext p)
     )
  => Proxy p
  -> Email
  -> m ()
forgotPassword p email = do
  accountIds <- runSelectReturningList $ select $ do
    a <- all_ $ providedP @AccountTable p
    guard_ $ _account_email a ==. val_ email
    pure $ pk a
  case accountIds of
    [accountId] -> do
      nonces <- runPgUpdateReturningList $ (`returning` _account_passwordResetNonce) $ update
        (providedP @AccountTable p)
        (\a -> _account_passwordResetNonce a <-. just_ current_timestamp_)
        (\a -> pk a ==. val_ accountId)
      case nonces of
        [Just nonce] -> do
          token <- signWithKey (providedP @AuthKey p) $ PasswordResetToken (accountId, nonce)
          providedP @SendAccountMessage p email $ AccountMessage_ResetPassword token
        _ -> fail "Unknown error"
    _ -> do
      providedP @SendAccountMessage p email AccountMessage_AccountDoesNotExist

ensureAccountExists
  :: forall m p
  .  ( MonadBeam Postgres m
     , Database Postgres (DbSchema (ProviderTypeContext p))
     , MonadFail m
     , MonadBeamInsertReturning Postgres m
     , p `Provides` AccountTable
     )
  => Proxy p
  -> Email
  -> m (Bool, PrimaryKey Account Identity)
ensureAccountExists p email = do
  existingAccountId <- runSelectReturningOne $ select $ fmap primaryKey $ filter_ (\x ->
    lower_ (_account_email x) ==. lower_ (val_ email)) $ all_ $ providedP @AccountTable p
  case existingAccountId of
    Just existing -> return (False, existing)
    Nothing -> do
      -- FUTURE: Use ON CONFLICT
      results <- runInsertReturningList $ insert (providedP @AccountTable p) $ insertExpressions
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
  :: forall m p
  .  ( MonadBeam Postgres m
     , EntropyGenerator m
     , p `Provides` AccountTable
     )
  => Proxy p
  -> PrimaryKey Account Identity
  -> Password
  -> m ()
setAccountPassword p aid password = do
  pw <- makePasswordHash password
  setAccountPasswordHash p aid pw

setAccountPasswordHash
  :: forall m p
  .  ( MonadBeam Postgres m
     , p `Provides` AccountTable
     )
  => Proxy p
  -> PrimaryKey Account Identity
  -> ByteString
  -> m ()
setAccountPasswordHash p aid hash = runUpdate $ update (providedP @AccountTable p)
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
  :: forall m p
  .  ( MonadBeam Postgres m
     , MonadBeamUpdateReturning Postgres m
     , p `Provides` AccountTable
     )
  => Proxy p
  -> PrimaryKey Account Identity
  -> m (Maybe UTCTime)
newNonce p aid = do
  a <- runUpdateReturningList $ update (providedP @AccountTable p)
    (\x -> _account_passwordResetNonce x <-. just_ current_timestamp_)
    (\x -> primaryKey x ==. val_ aid)
  pure $ case a of
    [acc] -> _account_passwordResetNonce acc
    _ -> Nothing
