{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-redundant-constraints #-}

module Rhyolite.Backend.Schema.TH
  ( deriveNewtypePersistBackend
  , makeDefaultKeyIdInt64
  , makeDefaultKeyIdSimple
  , mkRhyolitePersist
  , makePersistFieldNewtype
  , module Rhyolite.Backend.Schema
  ) where

import Control.Lens ((%~), _head)
import Control.Monad
import Control.Monad.State (mapStateT)
import Data.Char (toLower)
import Database.Groundhog
import Database.Groundhog.Core
import Database.Id.Groundhog.TH
import Data.Semigroup ((<>))
import Data.List (isPrefixOf)
import Database.Groundhog.TH (migrationFunction, namingStyle, mkDbFieldName, mkExprFieldName, mkExprSelectorName, mkPersist, defaultCodegenConfig)
import Database.Groundhog.TH.Settings (PersistDefinitions(..))
import Language.Haskell.TH

import Rhyolite.TH (conName)
import Rhyolite.Backend.Schema -- Not needed for this module, but without it, the generated code fails to compile in a way which is confusing, so we re-export it.

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
    functions = sequence
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

-- | Run Database.Groundhog.TH.mkPersist with rhyolite-specific defaults
--
-- Record field names are expected to look like _dataTypeName_fieldName
mkRhyolitePersist :: Maybe String -> PersistDefinitions -> Q [Dec]
mkRhyolitePersist migrationFunctionName = mkPersist $ defaultCodegenConfig
  { migrationFunction = migrationFunctionName
  , namingStyle =
    let ns = namingStyle defaultCodegenConfig
        dropPrefix p x = if p `isPrefixOf` x then drop (length p) x else error $ "mkRhyolitePersist: dropPrefix: expected string with prefix " <> show p <> ", got string " <> show x
    in ns { mkDbFieldName = \typeName cN conPos fieldName fieldPos ->
             mkDbFieldName ns typeName cN conPos (dropPrefix ("_" <> (_head %~ toLower) typeName <> "_") fieldName) fieldPos
          , mkExprFieldName = \typeName cN conPos fieldName fieldPos ->
             mkExprFieldName ns typeName cN conPos (dropPrefix "_" fieldName) fieldPos
          , mkExprSelectorName  = \typeName cN fieldName fieldPos ->
             mkExprSelectorName ns typeName cN (dropPrefix "_" fieldName) fieldPos
          }
  }

makePersistFieldNewtype :: Name -> Q [Dec]
makePersistFieldNewtype t = do
  TyConI (NewtypeD _ _ _ _ con _) <- reify t
  let c = conName con
  xName <- newName "x"
  [d| instance PersistField $(conT t) where
        persistName _ = $(stringE $ nameBase t)
        toPersistValues $(conP c [varP xName]) = toPersistValues $(varE xName)
        fromPersistValues pv = do
          (x, pv') <- fromPersistValues pv
          return ($(conE c) x, pv')
        dbType p $(conP c [varP xName]) = dbType p $(varE xName)
    |]
