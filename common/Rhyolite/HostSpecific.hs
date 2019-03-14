{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
#else
#endif
module Rhyolite.HostSpecific where

import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson hiding (Array)
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.AppendMap (AppendMap)
import qualified Data.AppendMap as Map
import Reflex.Dom.Core
import Rhyolite.Account
import Rhyolite.Request.TH (makeJson)
import Rhyolite.Schema
import Rhyolite.Sign

import qualified GHCJS.DOM.Types as DOM

#ifdef ghcjs_HOST_OS
import GHCJS.Types (JSString, JSVal)
import GHCJS.DOM (currentWindow)
import GHCJS.DOM.Types (Nullable, MouseEvent, nullableToMaybe, JSM, MonadJSM, liftJSM, ToJSVal(..), FromJSVal(..))
import qualified GHCJS.DOM.History as History
import qualified GHCJS.DOM.Window as Window
import GHCJS.Marshal.Pure (PToJSVal, pToJSVal)
import GHCJS.Concurrent
#else
import GHCJS.DOM.Types (MonadJSM, liftJSM, ToJSVal(..))
import Language.Javascript.JSaddle.Evaluate (eval)
import Language.Javascript.JSaddle.Object (call)
#endif

import qualified GHCJS.DOM.FormData as FD

{- Notes: Useful types and functions used for identifying files and transfering files from the frontend to backend -}

newtype FileId = FileId { unFileId :: Text}
  deriving (Eq, Ord, Show, Read)

instance ToJSON FileId where
  toJSON = toJSON . unFileId
  toEncoding = toEncoding . unFileId

instance FromJSON FileId where
  parseJSON = fmap FileId . parseJSON

instance HasId FileId

_FileId :: Iso' FileId Text
_FileId = iso unFileId FileId

data UploadTarget = UploadTarget_EcosystemMap
  deriving (Show)
makeJson ''UploadTarget

data Upload f = Upload
  { _upload_token :: Signed (AuthToken f)
  , _upload_target :: UploadTarget
  }
  deriving (Show)
makeJson ''Upload

data UploadError = UploadError_FileTooLarge (Maybe Text) | UploadError_WrongFileType Text (Maybe Text) | UploadError_OtherError Text
  deriving (Show)
makeJson ''UploadError

postFiles :: forall f g t m b.
           ( Traversable f, MonadWidget t m, DOM.IsBlob b
#ifdef ghcjs_HOST_OS
           , PToJSVal b
#endif
           )
          => Text -> Event t (f (Upload g, b)) -> m (Event t (f (Either UploadError FileId)))
postFiles uploadPath e = do
  responseE <- performMkRequestsAsync . ffor e . traverse $ \(extraParams, fs) -> do
    fd <- liftJSM $ FD.newFormData Nothing
    liftJSM $ append2 fd ("file" :: Text) fs
    iforM_ (paramMap extraParams) $ \k v -> do
      liftJSM $ append2 fd k v
    return $ xhrRequest "POST" uploadPath $ def & xhrRequestConfig_sendData .~ fd
  return $ fmap (fmap decodeResponse) responseE
  where
    decodeResponse r =
      case (decodeStrict . T.encodeUtf8 =<<) . _xhrResponse_responseText $ r of
        Nothing -> Left $ UploadError_OtherError "Couldn't decode response from server"
        Just rt -> rt

paramMap :: Upload f -> AppendMap Text Text
paramMap = Map.singleton "params" . T.decodeUtf8 . BSL.toStrict . encode

#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "$1[\"append\"]($2, $3)"
        js_append2 :: FD.FormData -> JSString -> JSVal -> IO ()

append2 :: (MonadIO m, DOM.ToJSString name, PToJSVal val)
        => FD.FormData -> name -> val -> m ()
append2 self name val = liftIO $ js_append2 self (DOM.toJSString name) (pToJSVal val)

#else
append2 :: (MonadJSM m, DOM.ToJSString name, ToJSVal val)
        => DOM.FormData -> name -> val -> m ()
append2 fd name val = liftJSM $ do
  f <- eval ("(function (x, y, z) { return x[\"append\"](y, z); })" :: Text)
  fd' <- toJSVal fd
  name' <- toJSVal name
  val' <- toJSVal val
  _ <- call f f [fd', name', val']
  return ()
#endif

