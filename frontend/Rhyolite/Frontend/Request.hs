{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Rhyolite.Frontend.Request where

import Control.Lens (iforM_)
import Data.Map as Map
import Data.Text (Text)
import Data.Traversable (for)
import Foreign.JavaScript.TH
#ifdef __GHCJS__
import GHCJS.Marshal.Pure
#endif
import GHCJS.DOM.File (getName)
import qualified GHCJS.DOM.FormData as FD
import GHCJS.DOM.Types (File, IsBlob,MonadJSM, liftJSM)
import Reflex.Dom.Core hiding (newXMLHttpRequest)

data FormValue blob = FormValue_Text Text
                    | FormValue_File blob (Maybe Text) -- maybe filename

postForms
  :: ( IsBlob blob, HasJSContext (Performable m), MonadJSM (Performable m)
     , PerformEvent t m, TriggerEvent t m
     , Traversable f)
  => Text
  -> Event t (f (Map Text (FormValue blob)))
  -> m (Event t (f XhrResponse))
postForms path payload = do
  performMkRequestsAsync $ ffor payload $ \fs -> for fs $ \u -> liftJSM $ do
    fd <- FD.newFormData Nothing
    iforM_ u $ \k v -> case v of
      FormValue_Text t -> FD.append fd k t
      FormValue_File b fn -> FD.appendBlob fd k b fn
    return $ xhrRequest "POST" path $ def & xhrRequestConfig_sendData .~ fd

fileToFormValue :: MonadJSM m => File -> m (FormValue File)
fileToFormValue f = do
  fn <- getName f
  return $ FormValue_File f $ Just fn
