-- | Backend utilities for using the 'Account' data type from "Rhyolite.Account".

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans -Wno-unused-matches #-}
{- NOTE: We add a -Wno-unused-matches because the code generated by groundhog has an unused variable
  GROUNDHOG ERROR -
  src/Rhyolite/Backend/Account.hs:35:1: warning: [-Wunused-matches]
    Defined but not used: ‘p’
  <no location info>: error:
  Failing due to -Werror.
-}

module Rhyolite.Backend.Account where

import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Crypto.PasswordStore
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Constraint.Extras
import Data.Constraint.Forall
import Data.Default
import Data.Functor.Identity
import Data.List.NonEmpty
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time
import Data.Typeable
import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.Generic.Sql.Functions
import Database.Groundhog.TH (defaultCodegenConfig, groundhog, mkPersist)
import Database.Id.Class
import Database.Id.Groundhog
import Database.Id.Groundhog.TH
import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Web.ClientSession as CS

import Rhyolite.Backend.DB
import Rhyolite.Backend.DB.Serializable (Serializable)
import qualified Rhyolite.Backend.DB.Serializable as Serializable
import Rhyolite.Backend.Email
import Database.PostgreSQL.Simple.Class
import Rhyolite.DB.NotifyListen.Groundhog
import Rhyolite.Backend.Sign (sign, signWithKey)

import Rhyolite.Account
import Rhyolite.Email
import Rhyolite.Route
import Rhyolite.Schema
import Rhyolite.Sign (MonadSign (SigningKey, askSigningKey), Signed)

mkPersist defaultCodegenConfig [groundhog|
  - entity: Account
    constructors:
      - name: Account
        uniques:
          - name: emailUnique
            type: index
            fields: [{expr: "lower(account_email::text)"}]
|]

makeDefaultKeyIdInt64 ''Account 'AccountKey

migrateAccount :: PersistBackend m => TableAnalysis m -> Migration m
migrateAccount tableInfo = migrate tableInfo (undefined :: Account)

-- | Returns whether a new account had to be created
ensureAccountExists
  :: ( PersistBackend m
     , SqlDb (PhantomDb m)
     , Has' ToJSON n Identity
     , ForallF ToJSON n
     , Psql m
     )
  => n (Id Account)
  -> Email
  -> m (Bool, Id Account)
ensureAccountExists nm email = do
  nonce <- getTime
  mPrevId <- fmap (listToMaybe . fmap toId) $ project AutoKeyField (lower Account_emailField ==. T.toLower email)
  case mPrevId of
    Just prevId -> return (False, prevId)
    Nothing -> do
      result <- insertByAll $ Account email Nothing (Just nonce)
      case result of
        -- TODO: Better way to handle errors?
        Left _ -> error "ensureAccountExists: Creating account failed"
        Right aid -> do
          let aid' = toId aid
          notify NotificationType_Insert nm aid'
          return (True, aid')

-- | Creates account if it doesn't already exist and sends pw email
ensureAccountExistsEmail
  :: ( PersistBackend m, MonadBase Serializable m
     , MonadSign m, SigningKey m ~ CS.Key
     , SqlDb (PhantomDb m)
     , Typeable f
     , ToJSON (f (Id Account))
     , Has' ToJSON n Identity
     , ForallF ToJSON n
     , Psql m
     )
  => n (Id Account)
  -> (Id Account -> f (Id Account))
  -> (Signed (PasswordResetToken f) -> Email -> m ()) -- pw reset email
  -> Email
  -> m (Bool, Id Account)
ensureAccountExistsEmail n = ensureAccountExistsEmail' (ensureAccountExists n)

-- | Creates account if it doesn't already exist and sends pw email Allows the
-- option for a custom "ensure account" creation function
ensureAccountExistsEmail'
  :: ( PersistBackend m, MonadBase Serializable m
     , MonadSign m, SigningKey m ~ CS.Key
     , Typeable f
     , ToJSON (f (Id Account)))
  => (Email -> m (Bool, Id Account))
  -> (Id Account -> f (Id Account))
  -> (Signed (PasswordResetToken f) -> Email -> m ()) -- pw reset email
  -> Email
  -> m (Bool, Id Account)
ensureAccountExistsEmail' ensureAccount decorateAccountId pwEmail email = do
  ret@(_, aid) <- ensureAccount email
  mNonce <- generateAndSendPasswordResetEmail decorateAccountId pwEmail aid
  forM_ mNonce $ \nonce -> do
    update [Account_passwordResetNonceField =. Just nonce] (Account_emailField ==. email)
  return ret

generatePasswordResetToken
  :: ( MonadSign m, SigningKey m ~ CS.Key
     , PersistBackend m
     , MonadBase Serializable m
     , Typeable f
     , ToJSON (f (Id Account))
     )
  => f (Id Account)
  -> m (Signed (PasswordResetToken f))
generatePasswordResetToken aid = do
  -- We can safely lift this 'IO' into 'Serializable' transactions because we don't
  -- actually care what the nonce is, so long as it's in the database by the time
  -- we're done.
  nonce <- getTime
  k <- askSigningKey
  liftBase $ Serializable.unsafeMkSerializable $ liftIO $ signWithKey k $ PasswordResetToken (aid, nonce)

generatePasswordResetTokenFromNonce
  :: ( MonadIO m
     , MonadSign m, SigningKey m ~ CS.Key
     , Typeable f
     , ToJSON (f (Id Account))
     )
  => f (Id Account)
  -> UTCTime
  -> m (Signed (PasswordResetToken f))
generatePasswordResetTokenFromNonce aid nonce = sign $ PasswordResetToken (aid, nonce)

setAccountPassword
  :: (PersistBackend m, MonadBase Serializable m)
  => Id Account
  -> Text
  -> m ()
setAccountPassword aid password = do
  -- We can safely make a password hash in 'Serializable' because retries
  -- will simply cause us to regenerate a random salt. Since it's random
  -- anyway we don't care how many times it runs.
  pw <- liftBase $ Serializable.unsafeMkSerializable $ liftIO $ makePasswordHash password
  update [ Account_passwordHashField =. Just pw
         , Account_passwordResetNonceField =. (Nothing :: Maybe UTCTime) ]
         (AutoKeyField ==. fromId aid)

makePasswordHash
  :: MonadIO m
  => Text
  -> m ByteString
makePasswordHash pw = do
  salt <- liftIO genSaltIO
  return $ makePasswordSaltWith pbkdf2 (2^) (encodeUtf8 pw) salt 14

resetPassword
  :: (PersistBackend m, MonadBase Serializable m)
  => Id Account
  -> UTCTime
  -> Text
  -> m (Maybe (Id Account))
resetPassword aid nonce password = runMaybeT $ do
  Just a <- get $ fromId aid
  if account_passwordResetNonce a == Just nonce
    then do
      setAccountPassword aid password
      return aid
    else fail "nonce mismatch"

login
  :: (PersistBackend m, SqlDb (PhantomDb m))
  => (Id Account -> m loginInfo)
  -> Email
  -> Text
  -> m (Maybe loginInfo)
login toLoginInfo email password = runMaybeT $ do
  (aid, a) <- MaybeT . fmap listToMaybe $ project (AutoKeyField, AccountConstructor) (lower Account_emailField ==. T.toLower email)
  ph <- MaybeT . return $ account_passwordHash a
  guard $ verifyPasswordWith pbkdf2 (2^) (encodeUtf8 password) ph
  lift $ toLoginInfo (toId aid)

loginByAccountId
  :: (PersistBackend m)
  => Id Account
  -> Text
  -> m (Maybe ())
loginByAccountId aid password = runMaybeT $ do
  a <- MaybeT . fmap listToMaybe $ project AccountConstructor (AutoKeyField ==. fromId aid)
  ph <- MaybeT . return $ account_passwordHash a
  guard $ verifyPasswordWith pbkdf2 (2^) (encodeUtf8 password) ph

generateAndSendPasswordResetEmail
  :: ( PersistBackend m
     , MonadBase Serializable m
     , MonadSign m, SigningKey m ~ CS.Key
     , Typeable f, ToJSON (f (Id Account))
     )
  => (Id Account -> f (Id Account))
  -> (Signed (PasswordResetToken f) -> Email -> m ())
  -> Id Account
  -> m (Maybe UTCTime)
generateAndSendPasswordResetEmail decorateAccountId pwEmail aid = do
  -- We can safely lift this 'IO' into a 'Serializable' transaction because
  -- any valid signature of the token will work. E.g. retrying will simply
  -- cause us to sign again which will still produce a valid signature.
  nonce <- getTime
  k <- askSigningKey
  prt <- liftBase $ Serializable.unsafeMkSerializable $ liftIO $ signWithKey k $ PasswordResetToken (decorateAccountId aid, nonce)
  ma <- get (fromId aid)
  forM ma $ \a -> do
    pwEmail prt (account_email a)
    return nonce

-- | Like 'generateAndSendPasswordResetEmail', but sets the nonce in the
-- database instead of just returning it.
generateAndSendPasswordResetEmailUpdateNonce
  :: ( PersistBackend m, MonadBase Serializable m
     , MonadSign m, SigningKey m ~ CS.Key, Typeable f, ToJSON (f (Id Account))
     )
  => (Id Account -> f (Id Account))
  -> (Signed (PasswordResetToken f) -> Email -> m ())
  -> Id Account
  -> m ()
generateAndSendPasswordResetEmailUpdateNonce f g aid = do
  nonce <- generateAndSendPasswordResetEmail f g aid
  void $ update [Account_passwordResetNonceField =. nonce] $ AutoKeyField ==. fromId aid

newAccountEmail
  :: (MonadRoute r m, Default r)
  => Text
  -> Text
  -> (AccountRoute f -> r)
  -> Signed (PasswordResetToken f)
  -> m Html
newAccountEmail productName productDescription f token = do
  passwordResetLink <- routeToUrl $ f $ AccountRoute_PasswordReset token
  emailTemplate productName
                Nothing
                (H.text $ "Welcome to " <> productName)
                (H.a H.! A.href (fromString $ show passwordResetLink) $ H.text "Click here to verify your email")
                (H.p $ H.text productDescription)

sendNewAccountEmail
  :: (MonadRoute r m, Default r, MonadEmail m)
  => Text
  -> Text
  -> Text
  -> Text
  -> (AccountRoute f -> r) -- How to turn AccountRoute into a route for a specific app
  -> Signed (PasswordResetToken f)
  -> Email
  -> m ()
sendNewAccountEmail senderName senderEmail productName productDescription f prt email = do
  body <- newAccountEmail productName productDescription f prt
  sendEmailFrom senderName senderEmail (email :| []) (productName <> " Verification Email") body

sendPasswordResetEmail
  :: (MonadEmail m, MonadRoute r m, Default r)
  => Text
  -> Text
  -> Text
  -> (AccountRoute f -> r)
  -> Signed (PasswordResetToken f)
  -> Email
  -> m ()
sendPasswordResetEmail senderName senderEmail productName f prt email = do
  passwordResetLink <- routeToUrl $ f $ AccountRoute_PasswordReset prt
  let lead = "You have received this message because you requested that your " <> productName <> " password be reset. Click the link below to create a new password."
      body = H.a H.! A.href (fromString $ show passwordResetLink) $ "Reset Password"
  sendEmailFrom senderName senderEmail (email :| []) (productName <> " Password Reset") =<< emailTemplate productName Nothing (H.text (productName <> " Password Reset")) (H.toHtml lead) body
