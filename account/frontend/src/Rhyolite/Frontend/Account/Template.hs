{-|
  Description:
    DOM templates for login/account components. These templates should
    be as free of application logic as possible.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Rhyolite.Frontend.Account.Template where

import Data.Default
import Data.Functor
import Data.Set (Set)
import Data.Text (Text)
import Obelisk.Route.Frontend
import Reflex.Dom.Core

data AccountViews t route m = AccountViews
  { _accountViews_login :: LoginConfig t route -> m (Login t)
  , _accountViews_createAccount :: CreateAccountConfig t route -> m (CreateAccount t)
  , _accountViews_finishAccountCreation :: FinishAccountCreationConfig t -> m (FinishAccountCreation t)
  , _accountViews_forgotPassword :: ForgotPasswordConfig t route -> m (ForgotPassword t)
  , _accountViews_resetPassword :: ResetPasswordConfig t route -> m (ResetPassword t)
  }

instance (SetRoute t route m, RouteToUrl route m, DomBuilder t m, PostBuild t m, Prerender t m) => Default (AccountViews t route m) where
  def = AccountViews
    { _accountViews_login = login
    , _accountViews_createAccount = createAccount
    , _accountViews_finishAccountCreation = finishAccountCreation
    , _accountViews_forgotPassword = forgotPassword
    , _accountViews_resetPassword = resetPassword
    }

-- * Login form

data SubmitStatus
   = SubmitStatus_Valid
   | SubmitStatus_Invalid
   | SubmitStatus_WaitingForResponse

data LoginResult
   = LoginResult_BadCredentials --TODO: Other server errors
   deriving (Show)

-- | Inputs to the 'login' template
data LoginConfig t route = LoginConfig
  { _loginConfig_createAccount :: route
  -- ^ The target of the "create account" link
  , _loginConfig_forgotPassword :: route
  -- ^ The target of the "forgot password" link
  , _loginConfig_submitStatus :: Dynamic t SubmitStatus
  -- ^ Form submission status (e.g., are we waiting for a response to load?)
  , _loginConfig_feedback :: Dynamic t (Maybe LoginResult)
  -- ^ User-facing feedback based on form state
  }

-- | Output of the 'login' template, including the login form elements.
data Login t = Login
  { _login_emailAddress :: Dynamic t Text
  , _login_password :: Dynamic t Text
  , _login_submit :: Event t ()
  }

login :: (DomBuilder t m, PostBuild t m, SetRoute t route m, RouteToUrl route m, Prerender t m) => LoginConfig t route -> m (Login t)
login cfg = do
  el "div" $ do
    el "div" $ do
      el "h2" $ text "Sign in to your account"
      el "p" $ do
        text "Or "
        routeLink (_loginConfig_createAccount cfg) $ do
          text "create a new account"
    el "form" $ do
      (email, password) <- el "div" $ do
        email <- el "div" $ do
          elAttr "label" ("for" =: "email-address") $ do
            text "Email address"
          fmap value $ inputElement $ def
            & initialAttributes .~ mconcat
                [ "id" =: "email-address"
                , "name" =: "email"
                , "type" =: "email"
                , "autofinish" =: "email"
                , "required" =: "true"
                , "placeholder" =: "someone@example.com"
                ]
        password <- el "div" $ do
          elAttr "label" ("for" =: "password") $ do
            text "Password"
          fmap value $ inputElement $ def
            & initialAttributes .~ mconcat
                [ "id" =: "password"
                , "name" =: "password"
                , "type" =: "password"
                , "autofinish" =: "current-password"
                , "required" =: "true"
                , "placeholder" =: "********"
                ]
        pure (email, password)
      el "div" $ do
        routeLink (_loginConfig_forgotPassword cfg) $ do
          text "Forgot your password?"
      display $ _loginConfig_feedback cfg --TODO
      submit <- el "div" $ do
        submitButton (_loginConfig_submitStatus cfg) $ do
          text "Sign in"
      pure $ Login
        { _login_emailAddress = email
        , _login_password = password
        , _login_submit = submit
        }

submitButton :: (DomBuilder t m, PostBuild t m) => Dynamic t SubmitStatus -> m () -> m (Event t ())
submitButton status child = do
  let attrs = status <&> \statusNow -> case statusNow of
        SubmitStatus_Valid -> mempty
        SubmitStatus_Invalid -> mconcat
          [ "style" =: "cursor: not-allowed"
          , "disabled" =: "disabled"
          ]
        SubmitStatus_WaitingForResponse -> mconcat
          [ "style" =: "cursor: wait"
          , "disabled" =: "disabled"
          ]
  (e, _) <- elDynAttr' "button" (attrs <> pure ("type" =: "button")) child
  return $ domEvent Click e

-- * New Account Form

-- | Inputs to the new account page template
data CreateAccountConfig t route = CreateAccountConfig
  { _createAccountConfig_login :: route
  -- ^ The target of the "login" link
  , _createAccountConfig_loading :: Dynamic t SubmitStatus
  -- ^ Form submission status (e.g., are we waiting for a response to load?)
  , _createAccountConfig_feedback :: Dynamic t (Maybe ()) --TODO
  -- ^ User-facing feedback based on form state
  }

-- | Outputs of the new account template, including form fields.
data CreateAccount t = CreateAccount
  { _createAccount_email :: Dynamic t Text
  , _createAccount_submit :: Event t ()
  }

createAccount :: (DomBuilder t m, PostBuild t m, RouteToUrl route m, SetRoute t route m, DomBuilder t m, PostBuild t m, Prerender t m) => CreateAccountConfig t route -> m (CreateAccount t)
createAccount cfg = do
  el "div" $ do
    el "div" $ do
      el "h2" $ text "Create a new account"
      el "p" $ do
        text "Or "
        routeLink (_createAccountConfig_login cfg) $ text "login to your account"
    el "form" $ do
      email <- el "div" $ do
        elAttr "label" ("for" =: "email") $ text "Email Address" --TODO: Generate unique IDs
        fmap value $ inputElement $ def
          & initialAttributes .~ mconcat
            [ "id" =: "email"
            , "name" =: "email"
            , "type" =: "email"
            , "autofinish" =: "email"
            , "required" =: "true"
            , "placeholder" =: "Email Address"
            ]
      --TODO: Show feedback
      submit <- el "div" $ do
        submitButton (_createAccountConfig_loading cfg) $ do
          text "Sign up"
      pure $ CreateAccount
        { _createAccount_email = email
        , _createAccount_submit = submit
        }

-- * Finish registration form

-- | Inputs to the registration completion form. Users reach this form after
-- verifying their emails.
data FinishAccountCreationConfig t = FinishAccountCreationConfig
  { _finishAccountCreationConfig_submitStatus :: Dynamic t SubmitStatus
  -- ^ Form submission status (e.g., are we waiting for a response to load?)
  , _finishAccountCreationConfig_feedback :: Dynamic t (Set ResetPasswordValidationError) --TODO: Not "ResetPassword"
  -- ^ User-facing feedback based on form state
  }

-- | Outputs of the registration completion form
data FinishAccountCreation t = FinishAccountCreation
  { _finishAccountCreation_password :: (Dynamic t Text, Dynamic t Text)
  , _finishAccountCreation_submit :: Event t ()
  }

-- | Form for completing registration (after email address has been verified)
finishAccountCreation :: (DomBuilder t m, PostBuild t m) => FinishAccountCreationConfig t -> m (FinishAccountCreation t)
finishAccountCreation cfg = do
  el "div" $ do
    el "div" $ do
      el "h2" $ text "Finish Creating Your Account"
    el "form" $ do
      passes <- passwordPair
      --TODO: Show feedback
      submit <- el "div" $ do
        submitButton (_finishAccountCreationConfig_submitStatus cfg) $ do
          text "Finish Account Creation"
      pure $ FinishAccountCreation
        { _finishAccountCreation_password = passes
        , _finishAccountCreation_submit = submit
        }

-- * Forgot password form

-- | Inputs to the new account page template
data ForgotPasswordConfig t route = ForgotPasswordConfig
  { _forgotPasswordConfig_login :: route
  -- ^ The target of the "login" link
  , _forgotPasswordConfig_loading :: Dynamic t SubmitStatus
  -- ^ Form submission status (e.g., are we waiting for a response to load?)
  , _forgotPasswordConfig_feedback :: Dynamic t (Maybe ()) --TODO
  -- ^ User-facing feedback based on form state
  }

-- | Outputs of the new account template, including form fields.
data ForgotPassword t = ForgotPassword
  { _forgotPassword_email :: Dynamic t Text
  , _forgotPassword_submit :: Event t ()
  }

forgotPassword :: (DomBuilder t m, PostBuild t m, RouteToUrl route m, SetRoute t route m, Prerender t m) => ForgotPasswordConfig t route -> m (ForgotPassword t)
forgotPassword cfg = do
  el "div" $ do
    el "div" $ do
      el "h2" $ text "Reset Password"
      el "p" $ do
        text "Or "
        routeLink (_forgotPasswordConfig_login cfg) $ text "login to your account"
    el "form" $ do
      email <- el "div" $ do
        elAttr "label" ("for" =: "email") $ text "Email Address"
        fmap value $ inputElement $ def
          & initialAttributes .~ mconcat
            [ "id" =: "email"
            , "name" =: "email"
            , "type" =: "email"
            , "autofinish" =: "email"
            , "required" =: "true"
            , "placeholder" =: "Email Address"
            ]
      --TODO: Output feedback
      submit <- el "div" $ do
        submitButton (_forgotPasswordConfig_loading cfg) $ do
          text "Send Reset Email"
      pure $ ForgotPassword
        { _forgotPassword_email = email
        , _forgotPassword_submit = submit
        }

-- * Set/Reset password form

data ResetPasswordValidationError
   = ResetPasswordValidationError_PasswordsDontMatch
   deriving (Show, Read, Eq, Ord)

-- | Inputs to the reset password page template
data ResetPasswordConfig t route = ResetPasswordConfig
  { _resetPasswordConfig_login :: route
  -- ^ The target of the "login" link
  , _resetPasswordConfig_submitStatus :: Dynamic t SubmitStatus
  -- ^ Form submission status (e.g., are we waiting for a response to load?)
  , _resetPasswordConfig_feedback :: Dynamic t (Set ResetPasswordValidationError)
  -- ^ User-facing feedback based on form state
  }

-- | Outputs of the reset password page template, including form fields
data ResetPassword t = ResetPassword
  { _resetPassword_passwordFields :: (Dynamic t Text, Dynamic t Text)
  , _resetPassword_submit :: Event t ()
  }

resetPassword :: (DomBuilder t m, PostBuild t m, RouteToUrl route m, SetRoute t route m, Prerender t m) => ResetPasswordConfig t route -> m (ResetPassword t)
resetPassword cfg = do
  el "div" $ do
    el "div" $ do
      el "h2" $ text "Set a new password"
      el "p" $ do
        text "Or "
        routeLink (_resetPasswordConfig_login cfg) $ text "login with your existing password"
    el "form" $ do
      passes <- passwordPair
      display $ _resetPasswordConfig_feedback cfg
      submit <- submitButton (_resetPasswordConfig_submitStatus cfg) $ do
        text "Set Password"
      pure $ ResetPassword
        { _resetPassword_passwordFields = passes
        , _resetPassword_submit = submit
        }

passwordPair :: DomBuilder t m => m (Dynamic t Text, Dynamic t Text)
passwordPair = divClass "rounded-md shadow-sm -space-y-px" $ do
  pass1 <- el "div" $ do
    elAttr "label" ("for" =: "password1") $ text "Password"
    inputElement $ def
      &  initialAttributes .~ ("id" =: "password1" <> "name" =: "password" <> "type" =: "password" <> "autofinish" =: "new-password" <> "required" =: "true" <> "placeholder" =: "********")
  pass2 <- el "div" $ do
    elAttr "label" ("for" =: "password2") $ text "Password (again)"
    inputElement $ def
      & initialAttributes .~ ("id" =: "password2" <> "name" =: "password" <> "type" =: "password" <> "autofinish" =: "new-password" <> "required" =: "true" <> "placeholder" =: "********")
  pure (value pass1, value pass2)
