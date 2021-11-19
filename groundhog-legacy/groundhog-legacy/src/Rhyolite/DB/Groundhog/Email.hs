{-|
Description:
  PersistBackend instance for EmailT
-}
{-# Language FlexibleContexts #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}
{-# Language UndecidableInstances #-}
{-# options_ghc -fno-warn-orphans #-}
module Rhyolite.DB.Groundhog.Email where

import Rhyolite.Email
import Control.Monad.Trans.Reader
import Rhyolite.DB.Groundhog.TH

deriveNewtypePersistBackend (\m -> [t| EmailT $m |]) (\m -> [t| ReaderT EmailEnv $m |]) 'EmailT 'unEmailT
