{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternGuards #-}
module Obelisk.Schema.TH (deriveFields, mkStandaloneDerivings, mkTrivialInstances) where

import Control.Lens (lens)
import Data.List
import Data.Char
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.Tabulation
import "template-haskell" Language.Haskell.TH
import Language.Haskell.TH.Datatype
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad

-- TODO: Get the more generally-useful stuff out of this module and into a package (th-extras? th-abstraction? something new?)

-- Do not export this type family, it must remain empty. It's used as a way to trick GHC into not unifying certain type variables.
type family Skolem :: k -> k

tyVarBndrName :: TyVarBndr -> Name
tyVarBndrName = \case
  PlainTV n -> n
  KindedTV n _ -> n

skolemize :: Set Name -> Type -> Type
skolemize rigids t = case t of
  ForallT bndrs cxt' t' -> ForallT bndrs cxt' (skolemize (Set.difference rigids (Set.fromList (map tyVarBndrName bndrs))) t')
  AppT t1 t2 -> AppT (skolemize rigids t1) (skolemize rigids t2)
  SigT t' k -> SigT (skolemize rigids t') k
  VarT v -> if Set.member v rigids
    then AppT (ConT ''Skolem) (VarT v)
    else t
  InfixT t1 n t2 -> InfixT (skolemize rigids t1) n (skolemize rigids t2)
  UInfixT t1 n t2 -> UInfixT (skolemize rigids t1) n (skolemize rigids t2)
  ParensT t' -> ParensT (skolemize rigids t')
  _ -> t

reifyInstancesWithRigids :: Set Name -> Name -> [Type] -> Q [InstanceDec]
reifyInstancesWithRigids rigids cls tys = reifyInstances cls (map (skolemize rigids) tys)

deriveFields :: Name -> String -> Q [Dec]
deriveFields typeOrConName fieldNameBase = do
  info <- reifyDatatype typeOrConName
  case datatypeCons info of
    [cinfo] | RecordConstructor fieldNames <- constructorVariant cinfo -> do
      tv <- newName "a"
      nf <- newName "f"
      nx <- newName "x"
      ny <- newName "y"
      let typeArgs = datatypeInstTypes info
          origType = foldl AppT (ConT (datatypeName info)) typeArgs
          fieldType = AppT (ConT $ ''Field) origType
          xType = VarT nx
          fieldDataDecl :: Dec = DataInstD [] (''Field) [origType, xType] Nothing cons []
          fieldDataDerivDecls = [StandaloneDerivD Nothing [] (AppT (ConT cls) (AppT fieldType (VarT tv))) | cls <- [''Eq, ''Ord, ''Show]]
          conNames = map (\fn -> mkName (fieldNameBase ++ (nameSuffix fn))) fieldNames
          cons = zipWith (\cn ty -> GadtC [cn] [] (AppT fieldType ty)) conNames (constructorFields cinfo)
          hasFieldsInstDecl = InstanceD Nothing [] (AppT (ConT ''HasFields) origType) [tabulateFieldsDecl, traverseWithFieldDecl, indexFieldDecl, fieldDataDecl]
          tabulateFieldsDecl = FunD 'tabulateFieldsA [Clause [VarP nf] (NormalB body) []]
            where body = foldl combine apCons apArgs
                  combine a b = (AppE (AppE (VarE '(<*>)) a) b)
                  apCons = (AppE (VarE 'pure) (ConE (constructorName cinfo)))
                  apArgs = [ AppE (VarE nf) (ConE cn) | cn <- conNames ]
          traverseWithFieldDecl = FunD 'traverseWithField [Clause [VarP nf, VarP nx] (NormalB body) []]
            where body = case zip conNames fieldNames of
                    [] -> AppE (VarE 'pure) conExp
                    cf:cfs -> foldl (\e1 e2 -> AppE (AppE a e1) e2) (AppE (AppE d conExp) (applyFun cf)) (map applyFun cfs)
                  applyFun (cn, fn) = AppE (AppE (VarE nf) (ConE cn)) (AppE (VarE fn) (VarE nx))
                  conExp = ConE (constructorName cinfo)
                  d = VarE '(<$>)
                  a = VarE '(<*>)
          indexFieldDecl = FunD 'fieldLens [Clause [VarP nf] (NormalB body) []]
            where body = CaseE (VarE nf) $ map (\(fn, cn) -> Match (ConP cn []) (NormalB (makeLens fn)) []) $ zip fieldNames conNames
                  makeLens fn = (AppE (AppE (VarE 'lens) (VarE fn)) (setter fn))
                  setter fn = LamE [VarP nx, VarP ny] $ RecUpdE (VarE nx) [(fn, VarE ny)]
          nameSuffix fn = case stripPrefix ("_" ++ onHead toLower (nameBase typeOrConName) ++ "_") (nameBase fn) of
            Nothing -> case fieldNames of
              _:[] -> "" -- TODO: check that field name looks like unConstructName
              _ -> error "deriveFields: Field names should be use the format _typeName_fieldName."
            Just x -> "_" ++ onHead toUpper x
      instances <- concat <$> sequenceA
        [ deriveGEq fieldDataDecl
        , deriveGCompare fieldDataDecl
        , deriveGShow fieldDataDecl
        ]
      return $ [hasFieldsInstDecl] ++ fieldDataDerivDecls ++ instances
    _ -> error "deriveFields: Type must have exactly one constructor using record syntax to derive a Field/HasField instance."

-- Makes standalone derivings for the given list of classes and type, assuming that whenever there is not
-- already an instance of the class for a field's type, we will need to demand that an instance of the
-- class is provided for that type.
mkStandaloneDerivings :: [Name] -> Name -> Q [Dec]
mkStandaloneDerivings classNames typeOrConName = do
  info <- reifyDatatype typeOrConName
  let theType = datatypeType info
  fmap concat $ forM classNames $ \className -> do
    constraints <- mkFieldContext info className
    return [StandaloneDerivD Nothing constraints (AppT (ConT className) theType)]

-- Makes empty instances for the given list of classes and the given type, assuming that whenever there is not
-- already an instance of the class for a field's type, we will need to demand that an instance of the class
-- is provided for that type.
mkTrivialInstances :: [Name] -> Name -> Q [Dec]
mkTrivialInstances classNames typeOrConName = do
  info <- reifyDatatype typeOrConName
  let theType = datatypeType info
  fmap concat $ forM classNames $ \className -> do
    constraints <- mkFieldContext info className
    return [InstanceD Nothing constraints (AppT (ConT className) theType) []]

-- Given info about a datatype, and a class name, build a context consisting of constrained field types
-- whenever an appropriate instance of the given class does not already exist for the type.
mkFieldContext :: DatatypeInfo -> Name -> Q Cxt
mkFieldContext info className = do
  fmap concat . forM (datatypeCons info) $ \con ->
    fmap concat . forM (constructorFields con) $ \ty -> do
      reduceContext (Set.fromList $ map tyVarBndrName (datatypeVars info)) [AppT (ConT className) ty]

decomposeClassConstraint :: Type -> Maybe (Name, [Type])
decomposeClassConstraint ty = do
  (n, ts) <- decomposeClassConstraintRev ty
  return (n, reverse ts)
  where
    decomposeClassConstraintRev :: Type -> Maybe (Name, [Type])
    decomposeClassConstraintRev t = case t of
      AppT f t' -> do
        (n, ts) <- decomposeClassConstraintRev f
        return (n, t':ts)
      ConT n -> Just (n, [])
      _ -> Nothing

-- Given a set of rigid variables, and a context (Cxt ~ [Type]) possibly containing type class constraints,
-- find constraints for which there is a unique matching instance, and replace them by the context on the
-- instance, and recursively simplify that context in the same way.
reduceContext :: Set Name -> Cxt -> Q Cxt
reduceContext rigids cs = do
  fmap concat . forM cs $ \c -> case decomposeClassConstraint c of
    Nothing -> return [c]
    Just (className, args) -> do
      insts <- reifyInstancesWithRigids rigids className args
      case insts of
        [InstanceD _ cs' c' _] -> do
          s <- unifyTypes [c', c]
          reduceContext rigids (map (applySubstitution s) cs')
        _ -> return [c]


onHead :: (a -> a) -> [a] -> [a]
onHead _ [] = []
onHead f (x:xs) = f x : xs
