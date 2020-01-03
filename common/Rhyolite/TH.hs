-- | TH utilities to embed files in source code.

{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Rhyolite.TH where

import qualified Data.ByteString as B
import qualified Data.FileEmbed as FE
import qualified Data.Text as T
import Data.Text.Encoding
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.FilePath

conName :: Con -> Name
conName c = case c of
  NormalC n _ -> n
  RecC n _ -> n
  InfixC _ n _ -> n
  ForallC _ _ c' -> conName c'
  GadtC [n] _ _ -> n
  RecGadtC [n] _ _ -> n
  _ -> error "conName: GADT constructors with multiple names not yet supported"

qReadFile :: FilePath -> Q B.ByteString
qReadFile path = do
  l <- location
  let file = takeDirectory (loc_filename l) </> path
  qAddDependentFile file
  runIO $ B.readFile file

embedFile :: FilePath -> Q Exp
embedFile p = do
  l <- location
  FE.embedFile $ takeDirectory (loc_filename l) </> p

embedFileString :: FilePath -> Q Exp
embedFileString fp = appE [| T.unpack . decodeUtf8 |] (embedFile fp)
