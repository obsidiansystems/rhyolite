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
  , AccountKey
  , AccountSendMessage
  , withCtx
  , CtxField (..)
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

import Control.Lens
import Data.Reflection
import Data.Proxy
import Data.Vinyl hiding (Dict)
import Data.Vinyl.ARec
import Data.Vinyl.TypeLevel
import Data.Kind (Constraint)
import Data.Constraint

--TODO:
-- Make calling getCtx less awkward
-- Improve inference of `db` type when using AccountTable db
--   Should each ctx type only be allowed to have a single db type?
-- Improve error messages when ctxs are missing
-- Remove _accountContext_ functions
class Ctx ctx key where
  ctxValue :: CtxValue key

getCtx :: forall key ctx. Ctx ctx key => Proxy ctx -> CtxValue key
getCtx _ = ctxValue @ctx @key

data ARecCtx recVal

instance (Reifies recVal (ARec CtxField ctxItems), RecElem ARec key key ctxItems ctxItems (RIndex key ctxItems)) => Ctx (ARecCtx recVal) key where
  ctxValue = unCtxField $ reflect (Proxy @recVal) ^. rlens @key

type family MapCtx ctx (a :: [*]) :: Constraint where
  MapCtx ctx '[] = ()
  MapCtx ctx (h ': t) = (Ctx ctx h, MapCtx ctx t)

type family CtxValue (a :: *) :: *

withCtx
  :: ( NatToInt (RLength ctxItems)
     , ToARec ctxItems
     )
  => Rec CtxField ctxItems
  -> (forall recVal. Reifies recVal (ARec CtxField ctxItems) => Proxy (ARecCtx recVal) -> r)
  -> r
withCtx items r = reify (toARec items) $ \(Proxy :: Proxy recVal) -> r (Proxy @(ARecCtx recVal))

data AccountTable (db :: (* -> *) -> *)
type instance CtxValue (AccountTable db) = DatabaseEntity Postgres db (TableEntity Account)

_accountContext_table :: forall db ctx. Ctx ctx (AccountTable db) => Proxy ctx -> DatabaseEntity Postgres db (TableEntity Account)
_accountContext_table = getCtx @(AccountTable db)

data AccountKey
type instance CtxValue AccountKey = CS.Key

_accountContext_key :: forall ctx. Ctx ctx AccountKey => Proxy ctx -> CS.Key
_accountContext_key = getCtx @AccountKey

data AccountSendMessage (m :: * -> *)
type instance CtxValue (AccountSendMessage m) = Email -> AccountMessage -> m ()

_accountContext_sendMessage :: forall ctx m. Ctx ctx (AccountSendMessage m) => Proxy ctx -> Email -> AccountMessage -> m ()
_accountContext_sendMessage = getCtx @(AccountSendMessage m)

data AccountMessage
   = AccountMessage_FinishAccountCreation (Signed PasswordResetToken)
   | AccountMessage_AccountAlreadyExists
   | AccountMessage_ResetPassword (Signed PasswordResetToken)
   | AccountMessage_AccountDoesNotExist

handleAccountRequest
  :: forall db m ctx a
  .  ( MonadBeam Postgres m
     , Database Postgres db
     , EntropyGenerator m
     , MonadFail m
     , Ctx ctx (AccountTable db)
     , Ctx ctx AccountKey
     , Ctx ctx (AccountSendMessage m)
     )
  => Proxy ctx
  -> AccountRequest a
  -> m a
handleAccountRequest ctx = \case
  AccountRequest_Login email password -> login @db ctx email password
  AccountRequest_CreateAccount email -> createAccount @db ctx email
  AccountRequest_FinishAccountCreation token password -> finishAccountCreation @db ctx token password
  AccountRequest_ForgotPassword email -> forgotPassword @db ctx email
  AccountRequest_ResetPassword token password -> finishAccountCreation @db ctx token password --TODO: Not finishAccountCreation

-- FUTURE: Mitigate timing attacks, e.g. by verifying against a fake password
-- hash even if the record is not found, or by ensuring that all login requests
-- wait for the same amount of time.

-- | Attempts to login a user given some credentials.
login
  :: forall db m ctx
  .  ( MonadBeam Postgres m
     , Database Postgres db
     , EntropyGenerator m
     , Ctx ctx (AccountTable db)
     , Ctx ctx AccountKey
     )
  => Proxy ctx
  -> Email
  -> Password
  -> m (Maybe (Signed AuthToken))
login ctx email pass = runMaybeT $ do
  (aid, mPwHash) <- MaybeT $ fmap listToMaybe $ runSelectReturningList $ select $ do
    acc <- all_ $ _accountContext_table @db ctx
    guard_ $ lower_ (_account_email acc) ==. lower_ (val_ email)
    pure (_account_id acc, _account_password acc)
  pwHash <- MaybeT $ pure mPwHash
  guard $ verifyPasswordWith pbkdf2 (2^) (T.encodeUtf8 pass) pwHash
  lift $ signWithKey (_accountContext_key ctx) $ AuthToken $ AccountId aid

-- | Creates a new account
createAccount
  :: forall db m ctx
  .  ( MonadBeam Postgres m
     , EntropyGenerator m
     , Ctx ctx (AccountTable db)
     , Ctx ctx AccountKey
     , Ctx ctx (AccountSendMessage m)
     )
  => Proxy ctx
  -> Email
  -> m ()
createAccount ctx email = do
  --TODO: On conflict skip
  accountIds <- runPgInsertReturningList $ flip returning (\a -> (pk a, _account_passwordResetNonce a)) $ insert (_accountContext_table @db ctx) $ insertExpressions
    [ Account
        { _account_id = genRandomUuid_
        , _account_email = lower_ (val_ email)
        , _account_password = nothing_
        , _account_passwordResetNonce = just_ current_timestamp_
        }
    ]
  case accountIds of
    [(accountId, Just nonce)] -> do
      token <- signWithKey (_accountContext_key ctx) $ PasswordResetToken (accountId, nonce)
      _accountContext_sendMessage ctx email $ AccountMessage_FinishAccountCreation token
    _ -> do
      _accountContext_sendMessage ctx email AccountMessage_AccountAlreadyExists

-- FUTURE: Expiration policy for password reset tokens
finishAccountCreation
  :: forall db m ctx
  .  ( MonadBeam Postgres m
     , Database Postgres db
     , EntropyGenerator m
     , Ctx ctx (AccountTable db)
     , Ctx ctx AccountKey
     )
  => Proxy ctx
  -> Signed PasswordResetToken
  -> Password
  -> m (Either FinishAccountCreationError (Signed AuthToken))
finishAccountCreation ctx token password = case readSignedWithKey (_accountContext_key ctx) token of
  Nothing -> pure $ Left FinishAccountCreationError_InvalidToken
  Just (PasswordResetToken (accountId, nonceFromToken)) -> do
    nonces <- runSelectReturningList $ select $ do
      account <- all_ $ _accountContext_table @db ctx
      guard_ $ pk account ==. val_ accountId
      pure $ _account_passwordResetNonce account
    case nonces of
      [Just nonceFromDatabase]
        | nonceFromDatabase == nonceFromToken
          -> do
            setAccountPassword @db ctx accountId password
            fmap Right $ signWithKey (_accountContext_key ctx) $ AuthToken accountId
      _ -> pure $ Left FinishAccountCreationError_InvalidToken

-- | Sends the user a password reset email, only if their account already exists
forgotPassword
  :: forall db m ctx
  .  ( MonadBeam Postgres m
     , Database Postgres db
     , EntropyGenerator m
     , MonadFail m
     , Ctx ctx (AccountTable db)
     , Ctx ctx AccountKey
     , Ctx ctx (AccountSendMessage m)
     )
  => Proxy ctx
  -> Email
  -> m ()
forgotPassword ctx email = do
  accountIds <- runSelectReturningList $ select $ do
    a <- all_ $ _accountContext_table @db ctx
    guard_ $ _account_email a ==. val_ email
    pure $ pk a
  case accountIds of
    [accountId] -> do
      nonces <- runPgUpdateReturningList $ (`returning` _account_passwordResetNonce) $ update
        (_accountContext_table @db ctx)
        (\a -> _account_passwordResetNonce a <-. just_ current_timestamp_)
        (\a -> pk a ==. val_ accountId)
      case nonces of
        [Just nonce] -> do
          token <- signWithKey (_accountContext_key ctx) $ PasswordResetToken (accountId, nonce)
          _accountContext_sendMessage ctx email $ AccountMessage_ResetPassword token
        _ -> fail "Unknown error"
    _ -> do
      _accountContext_sendMessage ctx email AccountMessage_AccountDoesNotExist

newtype CtxField (t :: *) = CtxField { unCtxField :: CtxValue t }

ensureAccountExists
  :: forall db m ctx
  .  ( MonadBeam Postgres m
     , Database Postgres db
     , MonadFail m
     , MonadBeamInsertReturning Postgres m
     , Ctx ctx (AccountTable db)
     )
  => Proxy ctx
  -> Email
  -> m (Bool, PrimaryKey Account Identity)
ensureAccountExists ctx email = do
  existingAccountId <- runSelectReturningOne $ select $ fmap primaryKey $ filter_ (\x ->
    lower_ (_account_email x) ==. lower_ (val_ email)) $ all_ $ _accountContext_table @db ctx
  case existingAccountId of
    Just existing -> return (False, existing)
    Nothing -> do
      -- FUTURE: Use ON CONFLICT
      results <- runInsertReturningList $ insert (_accountContext_table @db ctx) $ insertExpressions
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
  :: forall db m ctx
  .  ( MonadBeam Postgres m
     , EntropyGenerator m
     , Ctx ctx (AccountTable db)
     )
  => Proxy ctx
  -> PrimaryKey Account Identity
  -> Password
  -> m ()
setAccountPassword ctx aid password = do
  pw <- makePasswordHash password
  setAccountPasswordHash @db ctx aid pw

setAccountPasswordHash
  :: forall db m ctx
  .  ( MonadBeam Postgres m
     , Ctx ctx (AccountTable db)
     )
  => Proxy ctx
  -> PrimaryKey Account Identity
  -> ByteString
  -> m ()
setAccountPasswordHash ctx aid hash = runUpdate $ update (_accountContext_table @db ctx)
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
  :: forall db m ctx
  .  ( MonadBeam Postgres m
     , MonadBeamUpdateReturning Postgres m
     , Ctx ctx (AccountTable db)
     )
  => Proxy ctx
  -> PrimaryKey Account Identity
  -> m (Maybe UTCTime)
newNonce ctx aid = do
  a <- runUpdateReturningList $ update (_accountContext_table @db ctx)
    (\x -> _account_passwordResetNonce x <-. just_ current_timestamp_)
    (\x -> primaryKey x ==. val_ aid)
  pure $ case a of
    [acc] -> _account_passwordResetNonce acc
    _ -> Nothing
