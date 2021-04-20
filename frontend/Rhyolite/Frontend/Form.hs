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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Rhyolite.Frontend.Form where

import Control.Lens ((%~), makeLenses, preview)
import Control.Monad
import Control.Monad.Except
import Data.Bifunctor
import Data.Functor.Compose
import Data.Map (Map)
import qualified Data.Map as Map
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
  -> (Dynamic t Text -> m (DynValidation t e a)) -- Validation
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
  :: (DomBuilder t m, HasValue w, Value w ~ Dynamic t v, MonadHold t m)
  => (Dynamic t v -> m (DynValidation t e a)) -- Validation
  -> m w -- Render input
  -> m (w, DynValidation t e a)
manageValidation validator renderInput = do
  input <- renderInput
  validated <- validator $ value input
  return (input, validated)

guardEither :: e -> Bool -> Either e ()
guardEither e cond = if cond then Right () else Left e

validateNonEmpty :: Text -> Validation () Text
validateNonEmpty m = fromEither $ do
  let txt = T.strip m
  guardEither () $ not $ T.null txt
  return txt

validateJust :: Maybe a -> Validation () a
validateJust = fromEither . maybe (Left ()) pure

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

-- | Configure how to perform validation of an input widget
--
-- - `e` is the error type of the validation
-- - `a` is the result type of the validation
-- - `v` is the value used (typically `Text`) internally by the widget
data ValidationConfig t m e a v = ValidationConfig
  { _validationConfig_feedback :: Either (Dynamic t e) (Dynamic t a) -> m ()
  -- ^ For displaying the error in the browser with manual styling.
  , _validationConfig_errorText :: e -> Text
  -- ^ For the base HTML form validation, in which errors are non-empty strings.
  , _validationConfig_validation :: Dynamic t v -> DynValidation t e a
  -- ^ Input is always being reevaluated, including when external dynamics
  -- "mixed in" with this change. But rather than pushing changes downstream,
  -- downstream needed to ask for them (poll) with the 'validate' field.
  , _validationConfig_validationM :: Maybe (Dynamic t v -> m (DynValidation t e a))
  -- ^ This validation allows for the use of monadic effects (e.g. ask a
  -- server). The results of `_validationConfig_validatation` and
  -- `_validationConfig_validationM` will be combined by `*>`.
  , _validationConfig_initialAttributes :: Map AttributeName Text
  , _validationConfig_validAttributes :: Map AttributeName Text
  , _validationConfig_invalidAttributes :: Map AttributeName Text
  , _validationConfig_initialValue :: v
  , _validationConfig_setValue :: Maybe (Event t v)
  , _validationConfig_validate :: Event t ()
  -- ^ When to show validations and open the gate so downstream gets a new
  -- result. Fresh errors is the price for fresh results.
  }

-- | Like mkValidationConfig but for monoidal widget values
defValidationConfig
  :: (DomBuilder t m, Monoid v)
  => ValidationConfig t m Text a v
defValidationConfig = mkValidationConfig mempty

-- | Make a ValidationConfig with base values.
mkValidationConfig
  :: DomBuilder t m
  => v
  -- ^ Initial value to use in the widget
  -> ValidationConfig t m Text a v
mkValidationConfig ini = ValidationConfig
  { _validationConfig_feedback = const blank
  , _validationConfig_errorText = id
  , _validationConfig_validation = const $ toDynValidation $ pure $ Left "Validation not configured"
  , _validationConfig_validationM = Nothing
  , _validationConfig_initialAttributes = mempty
  , _validationConfig_validAttributes = mempty
  , _validationConfig_invalidAttributes = mempty
  , _validationConfig_initialValue = ini
  , _validationConfig_setValue = Nothing
  , _validationConfig_validate = never
  }

data ValidationInput t m e a = ValidationInput
  { _validationInput_input :: InputElement EventResult (DomBuilderSpace m) t
  , _validationInput_value :: DynValidation t e a
  }

data ValidationTextArea t m e a = ValidationTextArea
  { _validationTextArea_input :: TextAreaElement EventResult (DomBuilderSpace m) t
  , _validationTextArea_value :: DynValidation t e a
  }

data ValidationDropdown t e a = ValidationDropdown
  { _validationDropdown_input :: Dropdown t (Maybe a)
  , _validationDropdown_value :: DynValidation t e a
  }

instance HasValue (ValidationInput t m e a) where
  type Value (ValidationInput t m e a) = DynValidation t e a
  value = _validationInput_value

instance HasValue (ValidationTextArea t m e a) where
  type Value (ValidationTextArea t m e a) = DynValidation t e a
  value = _validationTextArea_value

instance HasValue (ValidationDropdown t e a) where
  type Value (ValidationDropdown t e a) = DynValidation t e a
  value = _validationDropdown_value

instance Reflex t => HasDomEvent t (ValidationInput t m e a) en where
  type DomEventType (ValidationInput t m e a) en = DomEventType (InputElement EventResult m t) en
  domEvent en = domEvent en . _validationInput_input

instance Reflex t => HasDomEvent t (ValidationTextArea t m e a) en where
  type DomEventType (ValidationTextArea t m e a) en = DomEventType (TextAreaElement EventResult m t) en
  domEvent en = domEvent en . _validationTextArea_input

-- | Get the event that triggers when the dropdown selection changes
--
-- Use this function as there is no HasDomEvent instance for Reflex's Dropdown
validationDropdownChangeEvent :: Reflex t => ValidationDropdown t e a -> Event t ()
validationDropdownChangeEvent = void . _dropdown_change . _validationDropdown_input

validationInput
  :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m, Semigroup e)
  => ValidationConfig t m e a Text
  -> m (ValidationInput t m e a)
validationInput config = do
  (vi, feedback) <- validationInputWithFeedback config
  feedback
  return vi

validationTextArea
  :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m, Semigroup e)
  => ValidationConfig t m e a Text
  -> m (ValidationTextArea t m e a)
validationTextArea config = do
  (vi, feedback) <- validationTextAreaWithFeedback config
  feedback
  return vi

validationDropdown
  :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m, Semigroup e, Ord a)
  => Maybe a
  -> Dynamic t (Map (Maybe a) Text)
  -> ValidationConfig t m e a (Maybe a)
  -> m (ValidationDropdown t e a)
validationDropdown k0 options config = do
  (vi, feedback) <- validationDropdownWithFeedback k0 options config
  feedback
  return vi

validationInputWithFeedback
  :: forall t m e a
  .  ( DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m
     , Semigroup e , Reflex t
     )
  => ValidationConfig t m e a Text
  -> m (ValidationInput t m e a, m ())
validationInputWithFeedback config = validationCustomInputWithFeedback config ValidationInput $ \inputAttrs ->
  inputElement $ def
    & initialAttributes .~ _validationConfig_initialAttributes config
    & modifyAttributes .~ (fmap Just <$> inputAttrs)
    & inputElementConfig_initialValue .~ _validationConfig_initialValue config
    & inputElementConfig_setValue %~ maybe id const (_validationConfig_setValue config)

validationTextAreaWithFeedback
  :: forall t m e a
  .  ( DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m
     , Semigroup e , Reflex t
     )
  => ValidationConfig t m e a Text
  -> m (ValidationTextArea t m e a, m ())
validationTextAreaWithFeedback config = validationCustomInputWithFeedback config ValidationTextArea $ \inputAttrs ->
  textAreaElement $ def
    & initialAttributes .~ _validationConfig_initialAttributes config
    & modifyAttributes .~ (fmap Just <$> inputAttrs)
    & textAreaElementConfig_initialValue .~ _validationConfig_initialValue config
    & textAreaElementConfig_setValue %~ maybe id const (_validationConfig_setValue config)

validationDropdownWithFeedback
  :: forall t m e a
  .  ( DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m
     , Semigroup e, Reflex t, Ord a
     )
  => Maybe a
  -> Dynamic t (Map (Maybe a) Text)
  -> ValidationConfig t m e a (Maybe a)
  -> m (ValidationDropdown t e a, m ())
validationDropdownWithFeedback k0 options config = validationCustomInputWithFeedback config ValidationDropdown $
  \inputAttrs -> do
    attrs <- holdDyn (_validationConfig_initialAttributes config) inputAttrs
    let attrs' = Map.mapKeysMonotonic (\(AttributeName _ v) -> v) <$> attrs
    dropdown k0 options $ def
      & dropdownConfig_attributes .~ attrs'
      & dropdownConfig_setValue %~ maybe id const (_validationConfig_setValue config)

validationCustomInputWithFeedback
  :: forall t m e a v vi w
  .  ( DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m
     , Semigroup e, Reflex t, HasValue w, Value w ~ Dynamic t v
     )
  => ValidationConfig t m e a v
  -> (w -> DynValidation t e a -> vi)
  -> (Event t (Map AttributeName Text) -> m w)
  -> m (vi, m ())
validationCustomInputWithFeedback config mkVi w = do
  let validateL = _validationConfig_validate config
      validationL = combineValidators
        (_validationConfig_validation config) (_validationConfig_validationM config)
  rec (input, dValidated) <- manageValidation validationL $ w inputAttrs
      let eValidated = tagPromptlyDyn (fromDynValidation dValidated) validateL
          inputAttrs = ffor eValidated $ \case
            Left _ -> _validationConfig_invalidAttributes config
            Right _ -> _validationConfig_validAttributes config
  val <- eitherDyn $ fromDynValidation dValidated
  let feedback = dyn_ $ _validationConfig_feedback config <$> val
  return (mkVi input dValidated, feedback)

combineValidators
  :: (Reflex t, Monad m, Semigroup e)
  => (Dynamic t v -> DynValidation t e a)
  -> Maybe (Dynamic t v -> m (DynValidation t e a))
  -> Dynamic t v -> m (DynValidation t e a)
combineValidators pValidator mValidator t =
  case mValidator of
    Nothing -> pure $ pValidator t
    Just mv -> do
      r <- mv t
      pure $ pValidator t *> r

makeLenses ''ValidationConfig
