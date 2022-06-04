{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Network.Mail.Mime.Orphans where

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

deriving instance Read Mail.Address
