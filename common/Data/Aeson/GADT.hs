{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Aeson.GADT where

import Control.Monad
import Data.Aeson
import Data.Constraint.Forall
import Data.Functor.Classes
import Data.GADT.Compare
import Language.Haskell.TH

import Data.Constraint
import Data.Constraint.Extras
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum
import Data.Functor.Identity

import Data.Some (Some (..))

decCons :: Dec -> [Con]
decCons = \case
  DataD _ _ _ _ cs _ -> cs
  NewtypeD _ _ _ _ c _ -> [c]
  _ -> error "undefined"

conName :: Con -> Name
conName c = case c of
  NormalC n _ -> n
  RecC n _ -> n
  InfixC _ n _ -> n
  ForallC _ _ c' -> conName c'
  GadtC [n] _ _ -> n
  RecGadtC [n] _ _ -> n
  _ -> error "conName: GADT constructors with multiple names not yet supported"

conArity :: Con -> Int
conArity c = case c of
  NormalC _ ts -> length ts
  RecC _ ts -> length ts
  InfixC _ _ _ -> 2
  ForallC _ _ c' -> conArity c'
  GadtC _ ts _ -> length ts
  RecGadtC _ ts _ -> length ts

{-# DEPRECATED deriveGADTInstances "Use deriveJSONGADT instead" #-}
deriveGADTInstances :: Name -> DecsQ
deriveGADTInstances = deriveJSONGADT

deriveJSONGADT :: Name -> DecsQ
deriveJSONGADT n = do
  tj <- deriveToJSONGADT n
  fj <- deriveFromJSONGADT n
  eqt <- deriveEqTag n
  return (tj ++ fj  ++ eqt)

deriveToJSONGADT :: Name -> DecsQ
deriveToJSONGADT n = do
  x <- reify n
  let cons = case x of
       TyConI d -> decCons d
       _ -> error "undefined"
  [d|
    instance ToJSON ($(conT n) a) where
      toJSON r = $(caseE [|r|] $ map conMatchesToJSON cons)
    |]

deriveFromJSONGADT :: Name -> DecsQ
deriveFromJSONGADT n = do
  x <- reify n
  let cons = case x of
       TyConI d -> decCons d
       _ -> error "undefined"
  let wild = match wildP (normalB [|fail "deriveFromJSONGADT: Supposedly-complete GADT pattern match fell through in generated code. This shouldn't happen."|]) []
  [d|
    instance FromJSON (Some $(conT n)) where
      parseJSON v = do
        (tag', v') <- parseJSON v
        $(caseE [|tag' :: String|] $ map (conMatchesParseJSON [|v'|]) cons ++ [wild])
    |]

deriveEqTag :: Name -> DecsQ
deriveEqTag n = do
  x <- reify n
  let cons = case x of
       TyConI d -> decCons d
       _ -> error "undefined"
  [d|
    instance Eq1 f => EqTag $(conT n) f where
      eqTagged a b = $(caseE [|(a, b)|] $ concatMap conMatchesEqTagged cons)
    |]

-- | Generate all required matches (and some redundant ones...) for `eqTagged`
-- for some constructor
conMatchesEqTagged :: Con -> [MatchQ]
conMatchesEqTagged c = case c of
    ForallC _ _ c' -> conMatchesEqTagged c'
    GadtC _ tys _ -> forTypes (map snd tys)
    _ -> error "conMatchesEqTagged: Unmatched constructor type"
  where
    name = conName c
    forTypes ts =
      [ do
          as <- mapM (\_ -> newName "a") ts
          bs <- mapM (\_ -> newName "b") ts
          x <- newName "x"
          y <- newName "y"
          let compareTagFields = foldr (\(a, b) e -> [| $(varE a) == $(varE b) && $(e) |]) [| True |] (zip as bs)
          match
            (tupP [conP name (map varP as), conP name (map varP bs)])
            (normalB (lamE [varP x, varP y] [| $(compareTagFields) && eq1 $(varE x) $(varE y) |] ))
            []
      , match
          (tupP [conP name (map (const wildP) ts), wildP])
          (normalB [| \ _ _ -> False |])
          []
      ]

-- | Implementation of 'toJSON'
conMatchesToJSON :: Con -> MatchQ
conMatchesToJSON c = do
  let name = conName c
      base = nameBase name
      toJSONExp e = [| toJSON $(e) |]
  vars <- replicateM (conArity c) (newName "x")
  let body = toJSONExp $ tupE [ [| base :: String |] , tupE $ map (toJSONExp . varE) vars ]
  match (conP name (map varP vars)) (normalB body) []


-- | Implementation of 'parseJSON'
conMatchesParseJSON :: ExpQ -> Con -> MatchQ
conMatchesParseJSON e c = do
  let name = conName c
      match' = match (litP (StringL (nameBase name)))
  vars <- replicateM (conArity c) (newName "x")
  let forTypes _ = do
        let pat = tupP (map varP vars)
            conApp = foldl appE (conE name) (map varE vars)
            body = doE [ bindS pat [| parseJSON $e |]
                       , noBindS [| return (This $conApp) |]
                       ]
        match' (normalB body) []
  case c of
    ForallC _ _ c' -> conMatchesParseJSON e c'
    GadtC _ tys _ -> forTypes (map snd tys)
    NormalC _ tys -> forTypes (map snd tys)
    _ -> error "conMatchesParseJSON: Unmatched constructor type"
