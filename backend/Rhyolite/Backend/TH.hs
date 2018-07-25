{-# LANGUAGE TemplateHaskell, FlexibleInstances, TypeFamilies, UndecidableInstances #-}
module Rhyolite.Backend.TH where

import Language.Haskell.TH
import Control.Monad.State (mapStateT)
import Database.Groundhog
import Control.Monad
import Data.Monoid

deriveNewtypePersistBackend :: (TypeQ -> TypeQ) -> (TypeQ -> TypeQ) -> Name -> Name -> DecsQ
deriveNewtypePersistBackend toT fromT to from =
  liftM (:[]) $ liftM3 (InstanceD Nothing) (cxt [appT (conT ''PersistBackend) (fromT m), appT (conT ''Monad) m]) (appT (conT ''PersistBackend) (toT m)) $ liftM2 (<>) typeInstances functions
  where
    m = varT $ mkName "m"
    typeInstances = do
      phantomDbInst <- tySynInstD ''PhantomDb $ tySynEqn [toT m] $ appT (conT ''PhantomDb) (fromT m)
      tableAnalysisInst <- tySynInstD ''TableAnalysis $ tySynEqn [toT m] $ appT (conT ''TableAnalysis) (fromT m)
      return [phantomDbInst, tableAnalysisInst]
    n =: e = valD (varP n) (normalB e) []
    functions = sequence $
      [ 'insert =: [| $(conE to) . insert |]
      , 'insert_ =: [| $(conE to) . insert_ |]
      , 'insertBy =: [| \u v -> $(conE to) $ insertBy u v |]
      , 'insertByAll =: [| $(conE to) . insertByAll |]
      , 'replace =: [| \k v -> $(conE to) $ replace k v |]
      , 'replaceBy =: [| \u v -> $(conE to) $ replaceBy u v |]
      , 'select =: [| $(conE to) . select |]
      , 'selectAll =: [| $(conE to) selectAll |]
      , 'get =: [| $(conE to) . get |]
      , 'getBy =: [| $(conE to) . getBy |]
      , 'update =: [| \us c -> $(conE to) $ update us c |]
      , 'delete =: [| $(conE to) . delete |]
      , 'deleteBy =: [| $(conE to) . deleteBy |]
      , 'deleteAll =: [| $(conE to) . deleteAll |]
      , 'count =: [| $(conE to) . count |]
      , 'countAll =: [| $(conE to) . countAll |]
      , 'project =: [| \p o -> $(conE to) $ project p o |]
      , 'migrate =: [| \i v -> mapStateT $(conE to) $ migrate i v |]
      , 'executeRaw =: [| \c q p -> $(conE to) $ executeRaw c q p |]
      , 'queryRaw =: [| \c q p f -> $(conE to) $ queryRaw c q p $ \rp -> $(varE from) $ f $ $(conE to) rp |]
      , 'insertList =: [| $(conE to) . insertList |]
      , 'getList =: [| $(conE to) . getList |]
      ]
