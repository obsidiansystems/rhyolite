{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Rhyolite.Email.Orphans where

import ByteString.Aeson.Orphans ()
import Data.Aeson
import Data.Aeson.TH
import qualified Network.Mail.Mime as Mail

deriveJSON defaultOptions ''Mail.Address
deriveJSON defaultOptions ''Mail.Disposition
deriveJSON defaultOptions ''Mail.Encoding
deriveJSON defaultOptions ''Mail.Part
deriveJSON defaultOptions ''Mail.PartContent
deriveJSON defaultOptions ''Mail.Mail
