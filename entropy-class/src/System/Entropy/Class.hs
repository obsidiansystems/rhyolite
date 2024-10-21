{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module System.Entropy.Class where

import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.Trans
import Control.Monad.Writer
import Data.ByteString (ByteString)
import qualified System.Entropy

-- | A monad where entropy can be retrieved
class EntropyGenerator m where
  getEntropy :: Int -> m ByteString
  default getEntropy :: (Monad n, MonadTrans t, EntropyGenerator n, m ~ t n) => Int -> m ByteString
  getEntropy = lift . getEntropy

instance EntropyGenerator IO where
  getEntropy = System.Entropy.getEntropy

instance (EntropyGenerator m, Monad m) => EntropyGenerator (ReaderT r m)
instance (EntropyGenerator m, Monad m, Monoid w) => EntropyGenerator (WriterT w m)
instance (EntropyGenerator m, Monad m) => EntropyGenerator (StateT s m)
instance (EntropyGenerator m, Monad m) => EntropyGenerator (Strict.StateT s m)
