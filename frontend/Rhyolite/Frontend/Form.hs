{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Rhyolite.Frontend.Form where

import Control.Lens ((%~), makeLenses, preview)
import Control.Monad
import Control.Monad.Except
import Data.Functor.Compose
import Data.Map (Map)
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

newtype BehaviorValidation t e a = BehaviorValidation { unBehaviorValidation :: Compose (Behavior t) (Validation e) a }

deriving instance Reflex t => Functor (BehaviorValidation t e)
deriving instance (Reflex t, Semigroup e) => Applicative (BehaviorValidation t e)

fromDynValidation :: Reflex t => DynValidation t e a -> Dynamic t (Either e a)
fromDynValidation (DynValidation (Compose v)) = toEither <$> v

fromBehaviorValidation :: Reflex t => BehaviorValidation t e a -> Behavior t (Either e a)
fromBehaviorValidation (BehaviorValidation (Compose v)) = toEither <$> v

toDynValidation :: Reflex t => Dynamic t (Either e a) -> DynValidation t e a
toDynValidation e = DynValidation $ Compose $ fromEither <$> e

toBehaviorValidation :: Reflex t => Behavior t (Either e a) -> BehaviorValidation t e a
toBehaviorValidation e = BehaviorValidation $ Compose $ fromEither <$> e

pureDynValidation :: Reflex t => Dynamic t a -> DynValidation t e a
pureDynValidation a = toDynValidation $ Right <$> a

tagDynValidation :: Reflex t => DynValidation t e b -> Event t a -> Event t b
tagDynValidation (DynValidation (Compose b)) = push $ \_ -> preview _Success <$> sample (current b)

tagPromptlyDynValidation :: Reflex t => DynValidation t e b -> Event t a -> Event t b
tagPromptlyDynValidation (DynValidation (Compose b)) = attachPromptlyDynWithMaybe (\b' _ -> preview _Success b') b

manageValidity
  :: (DomBuilder t m, MonadHold t m, Prerender js m, PerformEvent t m)
  => Event t () -- When to validate
  -> (Behavior t Text -> BehaviorValidation t e a) -- Validation
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

-- | The returned dynamic is really an abuse of I dynamic, namely it shouldn't
-- be allowed to push anything. The one benefit of this vs 'fmap f . current' is
-- the result can be fed into 'attachPromptly' rather than 'tag' to avoid
-- loosing a frame.
weirdMap
  :: (Reflex t, MonadHold t m)
  => (Behavior t a -> Behavior t b)
  -> Dynamic t a
  -> m (Dynamic t b)
weirdMap f d = buildDynamic
  (sample $ f $ current d)
  (pushAlways (sample . f . pure) $ updated d)

manageValidation
  :: (DomBuilder t m, MonadHold t m)
  => (Behavior t Text -> BehaviorValidation t e a) -- Validation
  -> m (InputElement EventResult (DomBuilderSpace m) t) -- Render input
  -> m (InputElement EventResult (DomBuilderSpace m) t, DynValidation t e a)
manageValidation validator renderInput = do
  input <- renderInput
  validatedInput <- weirdMap (fromBehaviorValidation . validator) $ value input
  return (input, toDynValidation validatedInput)

guardEither :: e -> Bool -> Either e ()
guardEither e cond = if cond then Right () else Left e

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

data ValidationConfig t m e a = ValidationConfig
  { _validationConfig_feedback :: Either (Dynamic t e) (Dynamic t a) -> m ()
  -- ^ For displaying the error in the browser with manual styling.
  , _validationConfig_errorText :: e -> Text
  -- ^ For the base HTML form validation, in which errors are non-empty strings.
  , _validationConfig_validation :: Behavior t Text -> BehaviorValidation t e a
  -- ^ Can pull other data, but not be pushed by it.
  , _validationConfig_initialAttributes :: Map AttributeName Text
  , _validationConfig_validAttributes :: Map AttributeName Text
  , _validationConfig_invalidAttributes :: Map AttributeName Text
  , _validationConfig_initialValue :: Text
  , _validationConfig_setValue :: Maybe (Event t Text)
  , _validationConfig_validate :: Event t ()
  }

defValidationConfig :: DomBuilder t m => ValidationConfig t m Text a
defValidationConfig = ValidationConfig
  { _validationConfig_feedback = const blank
  , _validationConfig_errorText = id
  , _validationConfig_validation = const $ toBehaviorValidation $ pure $ Left "Validation not configured"
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
  let validation' = _validationConfig_validate config
  rec (input, dValidated0) <- manageValidation (_validationConfig_validation config) $ do
        inputElement $ def
          & initialAttributes .~ _validationConfig_initialAttributes config
          & modifyAttributes .~ inputAttrs
          & inputElementConfig_initialValue .~ _validationConfig_initialValue config
          & inputElementConfig_setValue %~ maybe id const (_validationConfig_setValue config)
      let eValidated = tagPromptlyDyn (fromDynValidation dValidated0) validation'
          inputAttrs = ffor eValidated $ \case
            Left _ -> fmap Just $ _validationConfig_invalidAttributes config
            Right _ -> fmap Just $ _validationConfig_validAttributes config
  let bValidated = current $ fromDynValidation dValidated0
  dValidated <- fmap toDynValidation $ buildDynamic (sample bValidated) eValidated
  val <- eitherDyn $ fromDynValidation dValidated
  dyn_ $ _validationConfig_feedback config <$> val
  return $ ValidationInput input dValidated

makeLenses ''ValidationConfig
