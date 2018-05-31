{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, QuasiQuotes, TemplateHaskell, FlexibleInstances, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleContexts, UndecidableInstances, ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-redundant-constraints #-}
module Rhyolite.Backend.Schema.TH where

import Control.Lens ((%~), _head)
import Data.Char (toLower)
import Data.Int (Int64)
import Database.Groundhog.Core
import Data.Semigroup ((<>))
import Data.List (isPrefixOf)
import Database.Groundhog.TH (migrationFunction, namingStyle, mkDbFieldName, mkExprFieldName, mkExprSelectorName, mkPersist, defaultCodegenConfig)
import Database.Groundhog.TH.Settings (PersistDefinitions(..))
import Language.Haskell.TH

import Rhyolite.Backend.Schema.Class

makeDefaultKeyIdInt64 :: Name -> Name -> Q [Dec]
makeDefaultKeyIdInt64 n k = do
  pv <- newName "pv"
  [d|
    instance IdDataIs $(conT n) Int64 => DefaultKeyId $(conT n) where
      toIdData _ dk = case $(lamE [conP k [varP pv]] (varE pv)) dk of
        PersistInt64 x -> x
        _ -> error "makeDefaultKeyIdInt64: pattern match failure (this should be impossible)"
      fromIdData _ = $(conE k) . PersistInt64
    |]

makeDefaultKeyIdSimple :: Name -> Name -> Q [Dec]
makeDefaultKeyIdSimple n k = do
  pv <- newName "pv"
  [d|
    instance DefaultKeyId $(conT n) where
      toIdData _ = $(lamE [conP k [varP pv]] (varE pv))
      fromIdData _ = $(conE k)
    |]


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

conName :: Con -> Name
conName c = case c of
  NormalC n _ -> n
  RecC n _ -> n
  InfixC _ n _ -> n
  ForallC _ _ c' -> conName c'
  GadtC [n] _ _ -> n
  RecGadtC [n] _ _ -> n
  _ -> error "conName: GADT constructors with multiple names not yet supported"
