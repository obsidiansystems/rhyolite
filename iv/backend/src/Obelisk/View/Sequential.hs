{-# LANGUAGE UndecidableInstances #-}
module Obelisk.View.Sequential where

import Prelude hiding (id, (.))

import Control.Category
import Control.Exception
import Data.These

import Obelisk.View.Coverable
import Obelisk.View.Interface


newtype IvBackwardSequential m a = IvBackwardSequential
  { -- | These are all together because they need to happen transactionally, i.e. all at the same underlying Time value
    _ivBackwardSequential_subscribeUnsubscribeRead :: These (These (Cov (Push a)) (Cov (Push a))) (Cov (Pull a)) -> m ()
  }

emptyIvBackwardSequential :: Applicative m => IvBackwardSequential m a
emptyIvBackwardSequential = IvBackwardSequential
  { _ivBackwardSequential_subscribeUnsubscribeRead = \_ -> pure ()
  }

data IvForwardSequential m a = IvForwardSequential
  { _ivForwardSequential_notify :: Push a -> m ()
  , _ivForwardSequential_readResponse :: Pull a -> m ()
  }

emptyIvForwardSequential :: Applicative m => IvForwardSequential m a
emptyIvForwardSequential = IvForwardSequential
  { _ivForwardSequential_notify = \_ -> pure ()
  , _ivForwardSequential_readResponse = \_ -> pure ()
  }


-- | A way of attaching to (and later detaching from) a pipeline (e.g., handle
-- client connection and disconnection)
newtype Registrar i = Registrar { runRegistrar :: IvForwardSequential IO i -> IO (IvBackwardSequential IO i, IO ()) }

-- run a registrar in a continuation which finalises the pipeline exactly when the continuation exits.
withRegistrar :: Registrar i -> IvForwardSequential IO i -> (IvBackwardSequential IO i -> IO r) -> IO r
withRegistrar r fwd k = bracket (runRegistrar r fwd) snd (k . fst)

newtype IvSequential a b = IvSequential (Registrar a -> Registrar b)

instance Category IvSequential where
  id = IvSequential id
  IvSequential f . IvSequential g = IvSequential $ f . g
