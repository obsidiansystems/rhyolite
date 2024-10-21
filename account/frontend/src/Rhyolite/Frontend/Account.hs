{-|
  Description:
    Login and registration components
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Rhyolite.Frontend.Account
  ( accountPages
  , login
  , createAccount
  , forgotPassword
  , resetPassword
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Fix
import Control.Monad.Trans
import Data.Functor.Compose
import Data.Semigroup (First(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Signed
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Validation
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Rhyolite.Account
import Rhyolite.Frontend.Account.Template

accountPages
  :: ( Adjustable t m
     , MonadHold t m
     , MonadFix m
     )
  => (forall a. Event t (AccountRequest a) -> m (Event t (Either e a)))
  -> (R AccountRoute -> route)
  -> AccountViews t route m
  -> RoutedT t (R AccountRoute) m (Event t (Signed AuthToken))
accountPages accountRequesting accountRoute views = fmap (fmap getFirst . snd) $ mapRoutedT runEventWriterT $ subRoute_ $ \case
  AccountRoute_Login -> finalSubRoute_ $ \() -> mdo
    (loginViewConfig, loggedIn) <- lift $ loginController accountRequesting accountRoute loginView
    loginView <- lift $ _accountViews_login views loginViewConfig
    tellEvent $ fmap First loggedIn
  AccountRoute_CreateAccount -> finalSubRoute_ $ \() -> mdo
    createAccountViewConfig <- lift $ createAccountController accountRequesting accountRoute createAccountView
    createAccountView <- lift $ _accountViews_createAccount views createAccountViewConfig
    pure ()
  AccountRoute_FinishAccountCreation -> askRoute >>= \token -> lift $ mdo
    (finishAccountCreationViewConfig, loggedIn) <- lift $ finishAccountCreationController accountRequesting accountRoute token finishAccountCreationView
    finishAccountCreationView <- lift $ _accountViews_finishAccountCreation views finishAccountCreationViewConfig
    tellEvent $ fmap First loggedIn
  AccountRoute_ForgotPassword -> finalSubRoute_ $ \() -> mdo
    forgotPasswordViewConfig <- lift $ forgotPasswordController accountRequesting accountRoute forgotPasswordView
    forgotPasswordView <- lift $ _accountViews_forgotPassword views forgotPasswordViewConfig
    pure ()
  AccountRoute_ResetPassword -> askRoute >>= \token -> lift $ mdo
    (resetPasswordViewConfig, loggedIn) <- lift $ resetPasswordController accountRequesting accountRoute token resetPasswordView
    resetPasswordView <- lift $ _accountViews_resetPassword views resetPasswordViewConfig
    tellEvent $ fmap First loggedIn

-- | Login to an existing account
loginController
  :: forall t m a e route
  .  ( MonadHold t m
     , Reflex t
     )
  => (forall a. Event t (AccountRequest a) -> m (Event t (Either e a)))
  -> (R AccountRoute -> route)
  -> Login t
  -> m (LoginConfig t route, Event t (Signed AuthToken))
loginController accountRequesting accountRoute t = do
  let email :: Dynamic t (Validation () Text)
      email = fmap Success $ _login_emailAddress t --TODO: Validate
      password = fmap Success $ _login_password t
      valid = getCompose $
        AccountRequest_Login <$> Compose email <*> Compose password
      submit = fmapMaybe (^? _Success) $ tag (current valid) (_login_submit t)
  response <- accountRequesting submit
  loading <- holdDyn False $ leftmost
    [ False <$ response
    , True <$ submit
    ]
  loginFailed <- holdDyn Nothing $ ffor response $ \case
    Left _err -> Just LoginResult_BadCredentials --TODO: These are general errors; the account-specific errors should be in the Right clause
    Right _ -> Nothing
  let submitStatus = liftA2 (\loadingNow validNow -> if loadingNow then SubmitStatus_WaitingForResponse else case validNow of { Success _ -> SubmitStatus_Valid ; Failure _ -> SubmitStatus_Invalid }) loading valid
  pure
    ( LoginConfig
      { _loginConfig_createAccount = accountRoute $ AccountRoute_CreateAccount :/ ()
      , _loginConfig_forgotPassword = accountRoute $ AccountRoute_ForgotPassword :/ ()
      , _loginConfig_submitStatus = submitStatus
      , _loginConfig_feedback = loginFailed
      }
    , fmapMaybe (^? _Right . _Just) response
    )

createAccountController
  :: forall t route m e.
     ( Reflex t
     , MonadHold t m
     )
  => (forall a. Event t (AccountRequest a) -> m (Event t (Either e a)))
  -> (R AccountRoute -> route)
  -> CreateAccount t
  -> m (CreateAccountConfig t route)
createAccountController accountRequesting accountRoute t = do
  let email = _createAccount_email $ t
      validEmail = fmap Success email --TODO: Validate
      valid = getCompose $ AccountRequest_CreateAccount <$> Compose validEmail
      submit = fmapMaybe (^? _Success) $ tag (current valid) (_createAccount_submit t)
  rsp <- accountRequesting $ submit
  response <- accountRequesting submit
  loading <- holdDyn False $ leftmost
    [ False <$ response
    , True <$ submit
    ]
  feedback <- holdDyn Nothing $ ffor response $ \case
    Left _err -> Just () --TODO: These are general errors; the account-specific errors should be in the Right clause
    Right _ -> Nothing
  let submitStatus = liftA2 (\loadingNow validNow -> if loadingNow then SubmitStatus_WaitingForResponse else case validNow of { Success _ -> SubmitStatus_Valid ; Failure _ -> SubmitStatus_Invalid }) loading valid
  --TODO: Tell the user to check their email
  pure $ CreateAccountConfig
    { _createAccountConfig_login = accountRoute $ AccountRoute_Login :/ ()
    , _createAccountConfig_loading = submitStatus
    , _createAccountConfig_feedback = feedback
    }

finishAccountCreationController
  :: forall t route m e.
     ( Reflex t
     , MonadHold t m
     )
  => (forall a. Event t (AccountRequest a) -> m (Event t (Either e a)))
  -> (R AccountRoute -> route)
  -> Dynamic t (Signed PasswordResetToken)
  -> FinishAccountCreation t
  -> m (FinishAccountCreationConfig t, Event t (Signed AuthToken))
finishAccountCreationController accountRequesting accountRoute token t = do
  let validPasswordPair = uncurry (liftA2 validatePasswordPair) $ _finishAccountCreation_password t
      valid = getCompose $ AccountRequest_FinishAccountCreation
        <$> Compose (fmap Success token)
        <*> Compose validPasswordPair
      submit = fmapMaybe (^? _Success) $ tag (current valid) (_finishAccountCreation_submit t)
  response <- accountRequesting submit
  loading <- holdDyn False $ leftmost
    [ False <$ response
    , True <$ submit
    ]
  let submitStatus = liftA2 (\loadingNow validNow -> if loadingNow then SubmitStatus_WaitingForResponse else case validNow of { Success _ -> SubmitStatus_Valid ; Failure _ -> SubmitStatus_Invalid }) loading valid
  pure
    ( FinishAccountCreationConfig
      { _finishAccountCreationConfig_submitStatus = submitStatus
      , _finishAccountCreationConfig_feedback = pure Set.empty --TODO
      }
    , fmapMaybe (^? _Right . _Right) response
    )

validatePasswordPair :: Text -> Text -> Validation (Set ResetPasswordValidationError) Text
validatePasswordPair pass1 pass2 =
  if pass1 == pass2
  then Success pass1
  else Failure $ Set.singleton ResetPasswordValidationError_PasswordsDontMatch

forgotPasswordController
  :: forall t route m e.
     ( Reflex t
     , MonadHold t m
     )
  => (forall a. Event t (AccountRequest a) -> m (Event t (Either e a)))
  -> (R AccountRoute -> route)
  -> ForgotPassword t
  -> m (ForgotPasswordConfig t route)
forgotPasswordController accountRequesting accountRoute t = do
  let email = _forgotPassword_email $ t
      validEmail = fmap Success email --TODO: Validate
      valid = getCompose $ AccountRequest_ForgotPassword <$> Compose validEmail
      submit = fmapMaybe (^? _Success) $ tag (current valid) (_forgotPassword_submit t)
  rsp <- accountRequesting $ submit
  response <- accountRequesting submit
  loading <- holdDyn False $ leftmost
    [ False <$ response
    , True <$ submit
    ]
  feedback <- holdDyn Nothing $ ffor response $ \case
    Left _err -> Just () --TODO: These are general errors; the account-specific errors should be in the Right clause
    Right _ -> Nothing
  let submitStatus = liftA2 (\loadingNow validNow -> if loadingNow then SubmitStatus_WaitingForResponse else case validNow of { Success _ -> SubmitStatus_Valid ; Failure _ -> SubmitStatus_Invalid }) loading valid
  --TODO: Tell the user to check their email
  pure $ ForgotPasswordConfig
    { _forgotPasswordConfig_login = accountRoute $ AccountRoute_Login :/ ()
    , _forgotPasswordConfig_loading = submitStatus
    , _forgotPasswordConfig_feedback = feedback
    }

resetPasswordController
  :: forall t route m e.
     ( Reflex t
     , MonadHold t m
     )
  => (forall a. Event t (AccountRequest a) -> m (Event t (Either e a)))
  -> (R AccountRoute -> route)
  -> Dynamic t (Signed PasswordResetToken)
  -> ResetPassword t
  -> m (ResetPasswordConfig t route, Event t (Signed AuthToken))
resetPasswordController accountRequesting accountRoute token t = do
  let validPasswordPair = uncurry (liftA2 validatePasswordPair) $ _resetPassword_passwordFields t
      valid = getCompose $ AccountRequest_ResetPassword
        <$> Compose (pure <$> token)
        <*> Compose validPasswordPair
      submit = fmapMaybe (^? _Success) $ tag (current valid) (_resetPassword_submit t)
  response <- accountRequesting submit
  loading <- holdDyn False $ leftmost
    [ False <$ response
    , True <$ submit
    ]
  let submitStatus = liftA2 (\loadingNow validNow -> if loadingNow then SubmitStatus_WaitingForResponse else case validNow of { Success _ -> SubmitStatus_Valid ; Failure _ -> SubmitStatus_Invalid }) loading valid
  pure
    ( ResetPasswordConfig
        { _resetPasswordConfig_login = accountRoute $ AccountRoute_Login :/ ()
        , _resetPasswordConfig_submitStatus = submitStatus
        , _resetPasswordConfig_feedback = pure Set.empty --TODO: Report form validation errors and error responses from the server
        }
    , fmapMaybe (^? _Right . _Right) response
    )

{-
-- | Produces a dynamic loading status based on start and end events
loadingRsp
  :: (Reflex t, MonadHold t m)
  => Event t a
  -> Event t b
  -> m (Dynamic t Status)
loadingRsp submit rsp = holdDyn Status_Ready $ leftmost
  [ Status_Loading <$ submit
  , Status_Ready <$ rsp
  ]

-- | Convert an email address into its textual representation
emailToText :: EmailValidate.EmailAddress -> Text
emailToText = T.decodeUtf8 . EmailValidate.toByteString
-}
