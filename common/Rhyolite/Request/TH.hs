{-# LANGUAGE TemplateHaskell #-}

module Rhyolite.Request.TH where

import Control.Monad (guard, replicateM)
import Data.Aeson.Types
import Data.Constraint (Dict (..))
import Data.List (isPrefixOf)
import Data.Semigroup ((<>))
import Language.Haskell.TH

import Rhyolite.HList (HList (HCons, HNil))
import Rhyolite.Request.Class
import Rhyolite.TH (conName)

makeJson :: Name -> DecsQ
makeJson n = do
  x <- reify n
  let base = nameBase n
      toBeStripped = base <> "_"
      modifyConName cn = if length cons == 1 then cn else if toBeStripped `isPrefixOf` cn then drop (length toBeStripped) cn else error $ "makeRequest: expecting name beginning with " <> show toBeStripped <> ", got " <> show cn
      cons = case x of
        TyConI d -> decCons d
        _ -> error "undefined"
      typeNames = map tvbName $ case x of
        TyConI d -> decTvbs d
        _ -> error "undefined"
  let wild = match wildP (normalB [|fail "invalid message"|]) []
  [d|
    instance ToJSON $(foldl appT (conT n) $ map varT typeNames)   where
      toJSON r = $(caseE [|r|] $ map (conToJson modifyConName) cons)
    instance FromJSON $(foldl appT (conT n) $ map varT typeNames) where
      parseJSON v = do
        (tag', v') <- parseJSON v
        $(caseE [|tag' :: String|] $ map (conParseJson modifyConName id [|v'|]) cons ++ [wild])
    |]

makeRequestForDataInstance :: Name -> Name -> DecsQ
makeRequestForDataInstance n n' = do
  x <- reify n
  let base = nameBase n
      toBeStripped = base <> "_"
      modifyConName cn = if length cons == 1 then cn else if toBeStripped `isPrefixOf` cn then drop (length toBeStripped) cn else error $ "makeRequest: expecting name beginning with " <> show toBeStripped <> ", got " <> show cn
      cons = case x of
                  (FamilyI _ dataInstances) -> do
                    (DataInstD _ _ (ConT m:_) _ xs _) <- dataInstances
                    guard $ m == n'
                    xs
                  _ -> error "undefined"
  let wild = match wildP (normalB [|fail "invalid message"|]) []
  [d|
    instance Request $(appT (conT n) (conT n')) where
      requestToJSON r = $(caseE [|r|] $ map (conToJson modifyConName) cons)
      requestParseJSON v = do
        (tag', v') <- parseJSON v
        $(caseE [|tag' :: String|] $ map (conParseJson modifyConName (\body -> [|SomeRequest <$> $body|]) [|v'|]) cons ++ [wild])
      requestResponseToJSON r = $(caseE [|r|] $ map (\c -> match (conP (conName c) $ replicate (conArity c) wildP) (normalB [|Dict|]) []) cons)
      requestResponseFromJSON r = $(caseE [|r|] $ map (\c -> match (conP (conName c) $ replicate (conArity c) wildP) (normalB [|Dict|]) []) cons)
    |]

conParseJson :: (String -> String) -> (ExpQ -> ExpQ) -> ExpQ -> Con -> MatchQ
conParseJson modifyName wrapBody e c = do
  let name = conName c
  varNames <- replicateM (conArity c) $ newName "f"
  let fields = map varE varNames
      tuple = foldr (\a b -> conP 'HCons [varP a, b]) (conP 'HNil []) varNames
      body = doE [ bindS tuple [|parseJSON $e|]
                 , noBindS [|return $(appsE (conE name : fields))|]
                 ]
  match (litP (StringL (modifyName $ nameBase name))) (normalB (wrapBody body)) []

conToJson :: (String -> String) -> Con -> MatchQ
conToJson modifyName c = do
  let name = conName c
      base = nameBase name
      tag' = modifyName base
  varNames <- replicateM (conArity c) $ newName "f"
  let tuple = foldr (\a b -> appsE [conE 'HCons, varE a, b]) (conE 'HNil) varNames
      body = [|toJSON (tag' :: String, toJSON $tuple)|]
  match (conP name $ map varP varNames) (normalB body) []

conArity :: Con -> Int
conArity c = case c of
  NormalC _ ts -> length ts
  RecC _ ts -> length ts
  InfixC _ _ _ -> 2
  ForallC _ _ c' -> conArity c'
  GadtC _ ts _ -> length ts
  RecGadtC _ ts _ -> length ts


decCons :: Dec -> [Con]
decCons d = case d of
  DataD _ _ _ _ cs _ -> cs
  NewtypeD _ _ _ _ c _ -> [c]
  _ -> error "undefined"

-- | Extracts the name from a type variable binder.
tvbName :: TyVarBndr -> Name
tvbName (PlainTV  name  ) = name
tvbName (KindedTV name _) = name


decTvbs :: Dec -> [TyVarBndr]
decTvbs d = case d of
  DataD _ _ tvbs _ _ _ -> tvbs
  NewtypeD _ _ tvbs _ _ _ -> tvbs
  _ -> error "undefined"
