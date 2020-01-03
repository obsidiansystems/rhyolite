-- | A mechanism to validate forms.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Rhyolite.Frontend.Form where

import Control.Lens ((%~), makeLenses, preview)
import Control.Monad
import Control.Monad.Except
import Data.Bifunctor
import Data.Functor.Compose
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
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
  (resp, ()) <- withRequestingStatus' input (fmap (, ()) . render)
  return resp

withRequestingStatus'
  :: (MonadFix m, MonadHold t m, Requester t m)
  => Dynamic t (Maybe (Request m a))
  -> (Dynamic t (RequestStatus (Request m a) (Response m a)) -> m (Event t (), other))
  -> m (Event t (Response m a), other)
withRequestingStatus' input render = do
  rec (fire, a) <- render status
      (status, request) <- requestingStatus response fire input
      response <- requesting request
  return (response, a)

newtype DynValidation t e a = DynValidation { unDynValidation :: Compose (Dynamic t) (Validation e) a }

deriving instance Reflex t => Functor (DynValidation t e)
deriving instance (Reflex t, Semigroup e) => Applicative (DynValidation t e)

instance Reflex t => Bifunctor (DynValidation t) where
  bimap f g (DynValidation (Compose v)) = DynValidation $ Compose $ bimap f g <$> v

fromDynValidation :: Reflex t => DynValidation t e a -> Dynamic t (Either e a)
fromDynValidation (DynValidation (Compose v)) = toEither <$> v

toDynValidation :: Reflex t => Dynamic t (Either e a) -> DynValidation t e a
toDynValidation e = DynValidation $ Compose $ fromEither <$> e

pureDynValidation :: Reflex t => Dynamic t a -> DynValidation t e a
pureDynValidation a = toDynValidation $ Right <$> a

tagDynValidation :: Reflex t => DynValidation t e b -> Event t a -> Event t b
tagDynValidation (DynValidation (Compose b)) = push $ \_ -> preview _Success <$> sample (current b)

tagPromptlyDynValidation :: Reflex t => DynValidation t e b -> Event t a -> Event t b
tagPromptlyDynValidation (DynValidation (Compose b)) = attachPromptlyDynWithMaybe (\b' _ -> preview _Success b') b

manageValidity
  :: ( DomBuilder t m, MonadHold t m, PerformEvent t m
     , Prerender js t m, RawInputElement (DomBuilderSpace m) ~ HTMLInputElement
     )
  => Event t () -- When to validate
  -> (Dynamic t Text -> DynValidation t e a) -- Validation
  -> (e -> Text) -- convert error to form for basic html validation
  -> m (InputElement EventResult (DomBuilderSpace m) t) -- Render input
  -> m (InputElement EventResult (DomBuilderSpace m) t, DynValidation t e a)
manageValidity validate' validator errorText renderInput = do
  v@(input, val) <- manageValidation validator renderInput
  prerender blank $ do
    let rawEl = _inputElement_raw input
    performEvent_ $ ffor (tagPromptlyDyn (fromDynValidation val) validate') $ \case
      Left typedMsg -> do
        let msg = errorText typedMsg
        setCustomValidity rawEl msg
        reportValidity_ rawEl
      _ -> setCustomValidity rawEl ("" :: Text) -- NOTE setting empty text is how the browser "clears" the error
  return v

manageValidation
  :: (DomBuilder t m, MonadHold t m)
  => (Dynamic t Text -> DynValidation t e a) -- Validation
  -> m (InputElement EventResult (DomBuilderSpace m) t) -- Render input
  -> m (InputElement EventResult (DomBuilderSpace m) t, DynValidation t e a)
manageValidation validator renderInput = do
  input <- renderInput
  return (input, validator $ value input)

guardEither :: e -> Bool -> Either e ()
guardEither e cond = if cond then Right () else Left e

validateNonEmpty :: Text -> Validation () Text
validateNonEmpty m = fromEither $ do
  let txt = T.strip m
  guardEither () $ not $ T.null txt
  return txt

validateEmail :: Text -> Validation () Text
validateEmail m = fromEither $ do
  ne <- toEither $ validateNonEmpty m
  let (_, xs) = T.breakOn "@" ne
  guardEither () $ T.length xs > 1
  return ne

validateUniqueName
  :: Text
  -> Set Text
  -> Validation () Text
validateUniqueName name otherNames = fromEither $ do
  guardEither () $ not $ Set.member name otherNames
  return name

data ValidationConfig t m e a = ValidationConfig
  { _validationConfig_feedback :: Either (Dynamic t e) (Dynamic t a) -> m ()
  -- ^ For displaying the error in the browser with manual styling.
  , _validationConfig_errorText :: e -> Text
  -- ^ For the base HTML form validation, in which errors are non-empty strings.
  , _validationConfig_validation :: Dynamic t Text -> DynValidation t e a
  -- ^ Input is always being reevaluated, including when external dynamics
  -- "mixed in" with this change. But rather than pushing changes downstream,
  -- downstream needed to ask for them (poll) with the 'validate' field.
  , _validationConfig_initialAttributes :: Map AttributeName Text
  , _validationConfig_validAttributes :: Map AttributeName Text
  , _validationConfig_invalidAttributes :: Map AttributeName Text
  , _validationConfig_initialValue :: Text
  , _validationConfig_setValue :: Maybe (Event t Text)
  , _validationConfig_validate :: Event t ()
  -- ^ When to show validations and open the gate so downstream gets a new
  -- result. Fresh errors is the price for fresh results.
  }

defValidationConfig :: DomBuilder t m => ValidationConfig t m Text a
defValidationConfig = ValidationConfig
  { _validationConfig_feedback = const blank
  , _validationConfig_errorText = id
  , _validationConfig_validation = const $ toDynValidation $ pure $ Left "Validation not configured"
  , _validationConfig_initialAttributes = mempty
  , _validationConfig_validAttributes = mempty
  , _validationConfig_invalidAttributes = mempty
  , _validationConfig_initialValue = ""
  , _validationConfig_setValue = Nothing
  , _validationConfig_validate = never
  }

data ValidationInput t m e a = ValidationInput
  { _validationInput_input :: InputElement EventResult (DomBuilderSpace m) t
  , _validationInput_value :: DynValidation t e a
  }

instance HasValue (ValidationInput t m e a) where
  type Value (ValidationInput t m e a) = DynValidation t e a
  value = _validationInput_value

instance Reflex t => HasDomEvent t (ValidationInput t m e a) en where
  type DomEventType (ValidationInput t m e a) en = DomEventType (InputElement EventResult m t) en
  domEvent en = domEvent en . _validationInput_input

validationInput
  :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => ValidationConfig t m e a
  -> m (ValidationInput t m e a)
validationInput config = do
  (vi, feedback) <- validationInputWithFeedback config
  feedback
  return vi

validationInputWithFeedback
  :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => ValidationConfig t m e a
  -> m (ValidationInput t m e a, m ())
validationInputWithFeedback config = do
  let validation' = _validationConfig_validate config
  rec (input, dValidated) <- manageValidation (_validationConfig_validation config) $ do
        inputElement $ def
          & initialAttributes .~ _validationConfig_initialAttributes config
          & modifyAttributes .~ inputAttrs
          & inputElementConfig_initialValue .~ _validationConfig_initialValue config
          & inputElementConfig_setValue %~ maybe id const (_validationConfig_setValue config)
      let eValidated = tagPromptlyDyn (fromDynValidation dValidated) validation'
          inputAttrs = ffor eValidated $ \case
            Left _ -> fmap Just $ _validationConfig_invalidAttributes config
            Right _ -> fmap Just $ _validationConfig_validAttributes config
  val <- eitherDyn $ fromDynValidation dValidated
  let feedback = dyn_ $ _validationConfig_feedback config <$> val
  return (ValidationInput input dValidated, feedback)

makeLenses ''ValidationConfig
