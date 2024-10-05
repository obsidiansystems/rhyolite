{-# LANGUAGE OverloadedStrings #-}
module Rhyolite.Frontend.Placeholder where

import Reflex.Dom.Core
import qualified Data.Text as T
import GHC.Stack

placeholderDiv :: (HasCallStack, DomBuilder t m) => m a -> m a
placeholderDiv child = do
  elAttr "div" ("style" =: "border: 5px dashed red; padding: 5px; font-family: \"Comic Sans MS\", cursive, sans-serif") $ do
    elAttr "h3" ("style" =: "color: red") $ text "Placeholder"
    el "pre" $ text $ T.pack $ prettyCallStack callStack
    child
