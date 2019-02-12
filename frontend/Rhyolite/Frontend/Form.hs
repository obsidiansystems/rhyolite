{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Rhyolite.Frontend.Form where

import Control.Monad
import Control.Monad.Except
import Data.Functor.Compose
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validation
import GHCJS.DOM.HTMLInputElement
import Reflex
import Reflex.Dom.Core

data RequestStatus req res
  = RequestStatus_NotReady
  | RequestStatus_Ready
  | RequestStatus_Started req
  | RequestStatus_Finished res
  deriving (Show, Eq)

requestingStatus
  :: (MonadFix m, MonadHold t m, Reflex t)
  => Event t response
  -> Event t ()
  -> Dynamic t (Maybe request)
  -> m (Dynamic t (RequestStatus request response), Event t request)
requestingStatus response fire input = do
  let isReady = ffor input $ \case
        Nothing -> RequestStatus_NotReady
        Just _ -> RequestStatus_Ready
  rec status <- fmap join $ holdDyn isReady $ leftmost
        [ constDyn . RequestStatus_Started <$> gatedRequest
        , constDyn . RequestStatus_Finished <$> response
        ]
      let notStarted = ffor (current status) $ \case
            RequestStatus_Started _ -> False
            _ -> True
          gatedRequest = gate notStarted $ tagMaybe (current input) fire
  return (status, gatedRequest)

withRequestingStatus
  :: (MonadFix m, MonadHold t m, Requester t m)
  => Dynamic t (Maybe (Request m a))
  -> (Dynamic t (RequestStatus (Request m a) (Response m a)) -> m (Event t ()))
  -> m (Event t (Response m a))
withRequestingStatus input render = do
  rec fire <- render status
      (status, request) <- requestingStatus response fire input
      response <- requesting request
  return response

newtype DynValidation t e a = DynValidation { unDynValidation :: Compose (Dynamic t) (Validation e) a }

deriving instance Reflex t => Functor (DynValidation t e)
deriving instance (Reflex t, Semigroup e) => Applicative (DynValidation t e)

fromDynValidation :: Reflex t => DynValidation t e a -> Dynamic t (Either e a)
fromDynValidation (DynValidation (Compose v)) = toEither <$> v

toDynValidation :: Reflex t => Dynamic t (Either e a) -> DynValidation t e a
toDynValidation e = DynValidation $ Compose $ fromEither <$> e

pureDynValidation :: Reflex t => Dynamic t a -> DynValidation t e a
pureDynValidation a = toDynValidation $ Right <$> a

manageValidity
  :: (DomBuilder t m, MonadHold t m, Prerender js m, PerformEvent t m)
  => Event t () -- When to validate
  -> (Text -> Either Text a) -- Validation
  -> m (InputElement EventResult (DomBuilderSpace m) t) -- Render input
  -> m (InputElement EventResult (DomBuilderSpace m) t, DynValidation t Text a)
manageValidity validate' validator renderInput = do
  v@(input, val) <- manageValidation validate' validator renderInput
  prerender blank $ do
    let rawEl = _inputElement_raw input
    performEvent_ $ ffor (tagPromptlyDyn (fromDynValidation val) validate') $ \case
      Left msg -> do
        setCustomValidity rawEl msg
        reportValidity_ rawEl
      _ -> setCustomValidity rawEl ("" :: Text) -- NOTE setting empty text is how the browser "clears" the error
  return v

manageValidation
  :: (DomBuilder t m, MonadHold t m)
  => Event t () -- When to validate
  -> (Text -> Either Text a) -- Validation
  -> m (InputElement EventResult (DomBuilderSpace m) t) -- Render input
  -> m (InputElement EventResult (DomBuilderSpace m) t, DynValidation t Text a)
manageValidation validate' validator renderInput = do
  input <- renderInput
  let currentVal = current $ value input
  validatedInput <- fmap (fmap validator) $ buildDynamic (sample currentVal) $ tagPromptlyDyn (value input) validate'
  return (input, toDynValidation validatedInput)

validateEmail :: Text -> Either Text Text
validateEmail m = do
  ne <- validateNonEmpty m
  case T.breakOn "@" ne of
    (_, xs) | T.length xs > 1 -> return ne
    _ -> throwError "Invalid email"

validateNonEmpty :: Text -> Either Text Text
validateNonEmpty m = do
  let txt = T.strip m
  case T.null txt of
    True -> throwError "Is empty"
    False -> return txt
