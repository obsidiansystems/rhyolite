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
import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Map as Map
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time.Clock
import Data.Traversable (for)
import Foreign.JavaScript.TH
import Rhyolite.Request.Common (decodeValue')
#ifdef __GHCJS__
import GHCJS.Marshal.Pure
import Rhyolite.Frontend.WebSocket (rawDecode)
#endif
import GHCJS.DOM.Enums (XMLHttpRequestResponseType (..))
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.File (getName)
import qualified GHCJS.DOM.FormData as FD
import GHCJS.DOM.Types (File, FormData, IsBlob, JSM, MonadJSM, PFromJSVal (..), XMLHttpRequest, liftJSM)
import GHCJS.DOM.XMLHttpRequest (getReadyState, getResponse, getResponseTextUnchecked, newXMLHttpRequest,
                                 openSimple, readyStateChange, send, sendFormData, setResponseType)

import qualified GHCJS.Buffer as JS (createFromArrayBuffer, toByteString)
import qualified JavaScript.TypedArray.ArrayBuffer as JS (MutableArrayBuffer, unsafeFreeze)
import Language.Javascript.JSaddle.Types (ghcjsPure)
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


validJSRef :: MonadJS x m => JSRef x -> m (Maybe (JSRef x))
validJSRef r = do
  u <- isJSUndefined r
  n <- isJSNull r
  return $ if u || n then Nothing else Just r

timeFrom :: UTCTime -> UTCTime -> Text
timeFrom t ct =
  let d = round $ diffUTCTime ct t
  in describe d
  where
    describeAbs :: Integer -> Text
    describeAbs n
      | n >= 86400 = let days = n `Prelude.div` 86400 in T.pack (show days) <> if days == 1 then " day " else " days "
      | n >= 3600 = let hrs = n `Prelude.div` 3600 in T.pack (show hrs) <> if hrs == 1 then " hour " else " hours "
      | n >= 60 = let mins = n `Prelude.div` 60 in T.pack (show mins) <> if mins == 1 then " minute " else " minutes "
      | n > 0 = T.pack (show n) <> if n == 1 then " second " else " seconds "
      | otherwise = ""
    describe :: Integer -> Text
    describe n = case n `compare` 0 of
      GT -> describeAbs n <> "ago"
      EQ -> "now"
      LT -> describeAbs (abs n) <> "from now"

mkRequestGeneric :: (MonadJSM m, MonadFix m)
                 => Maybe XMLHttpRequestResponseType
                 -> (XMLHttpRequest -> JSM r)
                 -> Text
                 -> (XMLHttpRequest -> m ())
                 -> Text
                 -> (r -> JSM a)
                 -> m XMLHttpRequest
mkRequestGeneric responseType convertResponse method sendFunc url cb = do
  xhr <- newXMLHttpRequest
  openSimple xhr method url
  maybe (return ()) (setResponseType xhr) responseType
  rec freeCallback <- liftJSM . on xhr readyStateChange . liftJSM $ do
                        readyState <- getReadyState xhr
                        when (readyState == 4) $ do
                          r <- convertResponse xhr
                          _ <- cb r
                          freeCallback
  _ <- sendFunc xhr
  return xhr

mkBinaryRequest :: (MonadFix m, MonadJSM m)
                => Text
                -> (XMLHttpRequest -> m ())
                -> Text
                -> (ByteString -> JSM a)
                -> m XMLHttpRequest
mkBinaryRequest = mkRequestGeneric (Just XMLHttpRequestResponseTypeArraybuffer) $
    bsFromArrayBuffer . pFromJSVal <=< getResponse

bsFromArrayBuffer :: MonadJSM m => JS.MutableArrayBuffer -> m ByteString
bsFromArrayBuffer ab = liftJSM $ JS.unsafeFreeze ab >>=
    ghcjsPure . JS.createFromArrayBuffer >>= ghcjsPure . JS.toByteString 0 Nothing

mkBinaryGet :: (MonadFix m, MonadJSM m)
            => Text
            -> (ByteString -> JSM a)
            -> m XMLHttpRequest
mkBinaryGet = mkBinaryRequest "GET" send

mkRequest :: (MonadJSM m, MonadFix m) => Text -> (XMLHttpRequest -> m ()) -> Text -> (Text -> JSM a) -> m XMLHttpRequest
mkRequest = mkRequestGeneric Nothing getResponseTextUnchecked

mkGet :: (MonadJSM m, MonadFix m) => Text -> (Text -> JSM a) -> m XMLHttpRequest
mkGet = mkRequest "GET" send

mkPost :: (MonadJSM m, MonadFix m) => Text -> FormData -> (Text -> JSM a) -> m XMLHttpRequest
mkPost url d = mkRequest "POST" (`sendFormData` d) url

#if 0
syncApi :: (Request r, ToJSON a, FromJSON a, MonadJS x m, MonadIO m, MonadFix m) => r a -> m a
syncApi req = do
  v <- liftIO $ newEmptyMVar
  asyncApi req $ putMVar v --TODO: Error handling
  liftIO $ takeMVar v

asyncApi :: (Request r, ToJSON a, FromJSON a, MonadJS x m, MonadIO m, MonadFix m) => r a -> (a -> IO b) -> m ()
asyncApi r f = asyncApiMaybe r $ \(Just x) -> f x

asyncApiMaybe :: (Request r, ToJSON a, FromJSON a, MonadJS x m, MonadIO m, MonadFix m) => r a -> (Maybe a -> IO b) -> m ()
asyncApiMaybe r f = do
  let reqJson = encode $ SomeRequest r
  _ <- mkPost "/api" (decodeUtf8 $ LBS.toStrict reqJson) $ \rspJson -> do
#ifdef __GHCJS__
    let mrsp = rawDecode $ pToJSVal rspJson
#else
    let mrsp = decodeValue' $ LBS.fromStrict $ encodeUtf8 rspJson
#endif
    liftIO $ f mrsp
  return ()

syncApiMaybe :: (Request r, ToJSON a, FromJSON a, MonadJS x m, MonadIO m, MonadFix m) => r a -> m (Maybe a)
syncApiMaybe req = do
  v <- liftIO $ newEmptyMVar
  asyncApiMaybe req $ putMVar v
  liftIO $ takeMVar v

requestingXhr :: (Request r, ToJSON a, FromJSON a, TriggerEvent t m, PerformEvent t m, HasJS x (WidgetHost m)) => Event t (r a) -> m (Event t a)
requestingXhr requestE = performEventAsync $ fmap (\r yield' -> liftJS $ asyncApi r yield') requestE

requestingXhrMany :: (Request r, ToJSON a, FromJSON a, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m), HasJS x (WidgetHost m), Traversable f) => Event t (f (r a)) -> m (Event t (f a))
requestingXhrMany requestsE = performEventAsync $ ffor requestsE $ \rs cb -> do
  resps <- forM rs $ \r -> do
    resp <- liftIO newEmptyMVar
    _ <- liftJS $ asyncApi r $ liftIO . putMVar resp
    return resp
  _ <- liftIO . forkIO $ cb =<< forM resps takeMVar
  return ()
#endif

--importJS Unsafe "decodeURIComponent(window['location']['search'])" "getWindowLocationSearch" [t| forall x m. MonadJS x m => m Text |]

-- | Decode a JSON value from Text.  In JavaScript, this will use JSON.parse for
-- greater efficiency.
decodeValueFromText :: FromJSON a => Text -> Maybe a
#ifdef __GHCJS__
decodeValueFromText = rawDecode . pToJSVal
#else
decodeValueFromText = decodeValue' . LBS.fromStrict . encodeUtf8
#endif
