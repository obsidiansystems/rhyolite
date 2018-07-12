{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Rhyolite.Frontend.FilePicker.Dropbox where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Lens.Operators ((^.))
import Control.Monad (join)
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHCJS.DOM.Types (JSM, MonadJSM, fromJSValUnchecked, liftJSM)
import Language.Javascript.JSaddle.Object (fun, js, js0, js1, jsg, jss, obj)
import Language.Javascript.JSaddle.Value (maybeNullOrUndefined, valToText)

import Reflex.Dom.Core

data PickerOptions = PickerOptoins
  deriving (Eq, Generic, Show)

pickerWidget
  :: ( TriggerEvent t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     )
  => Event t PickerOptions -> m (Event t (Either Text Text))
pickerWidget optsEvt = do
  (onResultEvent, onResultCallback) <- newTriggerEvent
  performEvent_ $ ffor optsEvt $ \opts -> liftJSM $
    choose opts (liftIO . onResultCallback)
  pure onResultEvent

choose :: PickerOptions -> (Either Text Text -> JSM ()) -> JSM ()
choose opts onResult = do
  handleMVar <- liftIO newEmptyMVar
  o <- optsAsObj
  o ^. jss (t_ "success") (fun $ \_ args -> case args of
    (files : _) ->
      onResult $ Left "todo"
    _ -> liftIO $ putStrLn "success callback: unexpected number of arguments")
  where
    optsAsObj = do
      o <- opts
      -- TODO: fill in
      pure o
    t_ :: Text -> Text
    t_ = id
