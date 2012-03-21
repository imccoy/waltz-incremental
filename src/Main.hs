{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad
import qualified Data.Data as Data
import Data.Either
import qualified Data.List as List
import Data.Maybe
import Debug.Trace
import System.Directory (removeFile)
import System.IO
import System.Environment (getArgs)
import System.Exit

import Outputable

import Bag
import BasicTypes 
import GHC.Paths ( libdir )
import Coercion
import CoreLint
import CoreSyn
import MkCore
import DataCon
import ErrUtils
import FastString ( uniqueOfFS )
import HscTypes hiding (lookupDataCon)
import HscMain
import HsDecls
import HsPat
import IdInfo
import Literal
import MkId
import Module
import MonadUtils
import Name hiding (varName)
import NameEnv
import OccName hiding (varName)
import qualified OccName as OccName
import RdrName
import Var
import Type
import TyCon
import Unique

import DynFlags

import GHC hiding (exprType)

mutantCoreModule mod dm = dm { dm_core_module = mutantModGuts mod (dm_core_module dm) }
mutantModGuts mod mg = mg { mg_binds = mutantCoreBinds (mg_binds mg)
                          , mg_types = mutantTypeEnv (mg_types mg)
                          , mg_dir_imps  = mutantDeps (mg_dir_imps mg) mod
                          }

mutantDeps imps mod = extendModuleEnv imps mod [(mkModuleName "Radtime", False, noSrcSpan)]

mutantCoreBinds binds = binds ++ (map mutantCoreBind binds)

interlace :: [a] -> [a] -> [a]
interlace [] [] = []
interlace (a:as) (b:bs) = a:b:(interlace as bs)

map2 :: (a -> c) -> (b -> d) -> [(a, b)] -> [(c, d)]
map2 f g = map (\(a, b) -> (f a, g b))

mutantCoreBind :: CoreBind -> CoreBind
mutantCoreBind corebind@(NonRec name exp) 
  = NonRec (mutantCoreBndr name) (mutantExp exp)
mutantCoreBind corebinds@(Rec name_exps)
  = Rec $ map2 mutantCoreBndr mutantExp name_exps

mutantCoreBndr :: CoreBndr -> CoreBndr
mutantCoreBndr = mutantId

-- Whatever idDetails is before we mutate it, it's a VanillaId afterwards. 
-- Worker/wrapper's aren't an exception, because we just mutate their 
-- names and mkDataConIDs handles the ID construction.
mutantId var = mk var' type_'
  where var' = (mutantName $ varName var)
        type_' = (mutantType $ varType var)
        mk n t | isTcTyVar var  = mkTcTyVar n t (tcTyVarDetails var)
               | isGlobalId var = mkGlobalVar VanillaId n t vanillaIdInfo
               | isLocalVar var = mkLocalVar  VanillaId n t vanillaIdInfo

mutantNameIntoSpace :: Name -> NameSpace -> String -> Name
mutantNameIntoSpace oldName nameSpace suffix
  | isInternalName oldName = mkInternalName unique occName (nameSrcSpan oldName)
  | isExternalName oldName = mkExternalName unique (adaptModule $ nameModule oldName)
                                            occName (nameSrcSpan oldName)
  | isWiredInName oldName  = mkSystemName unique occName 
  | otherwise              = mkSystemName unique occName 
  where oldNameString = occNameString $ nameOccName $ oldName
        nameString | oldNameString == "[]" = "BuiltinList_" ++ suffix
                   | otherwise             = oldNameString ++ "_" ++ suffix
        occName = mkOccName nameSpace nameString
        unique = getUnique occName
        adaptModule mod | modulePackageId mod == primPackageId = radtime
                        | otherwise                            = mod
        radtime = mkModule mainPackageId (mkModuleName "Radtime") 


mutantName oldName = mutantNameIntoSpace oldName
                                         (occNameSpace $ nameOccName oldName)
                                         "incrementalised"

mutantExp (Var id) = Var $ mutantCoreBndr id
mutantExp (Lit lit) = Lit lit
mutantExp (App expr arg) = App (mutantExp expr) (mutantExp arg)
-- for \ a -> b, need to check if a is a (incrementalize_type a)_hoist. 
-- If so, produce a (incrementalize_type b)_identity.
mutantExp (Lam id expr) = expr'
  where expr' | isTyVar id = trace ("AAA" ++ (showSDocDebug $ ppr id) ++ "     " ++ (showSDocDebug $ ppr $ mutantCoreBndr id)  ++ "BBB") $ Lam id $ Lam (mutantCoreBndr id) (mutantExp expr)
              | otherwise  = Lam (mutantCoreBndr id) (mutantExp expr) 
              | otherwise  = Lam (mutantCoreBndr id)
                                 (Case (Var $ mutantCoreBndr id)
                                       (mutantCoreBndr id) 
                                       oType
                                       [def, hoist])
        def = (DEFAULT, [], mutantExp expr)
        hoist = (DataAlt $ lookupDataCon iType AddConHoist
                ,[]
                ,mkCoreConApps (lookupDataCon oType AddConIdentity) []
                )
        iType = mutantType $ varType id
        oType = exprType $ mutantExp expr
mutantExp (Let bind expr) = Let (mutantCoreBind bind) (mutantExp expr)
mutantExp c@(Case expr id type_ alts) = Case (mutantExp expr)
                                             (mutantCoreBndr id)
                                             (mutantType type_)
                                             ((replaceAlt c)
                                              :(mutantAlts alts))
mutantExp (Cast expr coercion) = Cast (mutantExp expr) (mutantType coercion)
mutantExp (Type type_) = Type (mutantType type_)
mutantExp (Note note expr) = Note note (mutantExp expr)

mutantAlts _ = [] -- map mutantAlt 
replaceAlt c@(Case expr id type_ alts)
  = (DataAlt $ lookupDataCon (mutantType $ exprType expr) AddConReplacement
    ,[exprVar expr]
    ,App (foldl (\e t -> App e (Type t))
                (Var $ dataConWorkId con)
                (tyConAppArgs $ mutantType type_))
         c)
  where con = lookupDataCon (mutantType type_) AddConReplacement


mutantTypeEnv env = extendTypeEnvList env (map mutantTyThing $ typeEnvElts env)
mutantTyThing (AnId id) = AnId $ mutantId id
mutantTyThing (ADataCon con) = ADataCon $ mutantDataCon con
mutantTyThing (ATyCon con) = ATyCon $ mutantTyCon con
mutantTyThing (AClass cls) = AClass $ mutantClass cls


mutantType (getTyVar_maybe -> Just tyVar)
  = mkTyVarTy $ mutantTyVar tyVar
mutantType (splitAppTy_maybe -> Just (a, b))
  | isApp a   = mkAppTy (mutantType a) (mutantType b)
  | otherwise = mkAppTy (mkAppTy (mutantType a) b)
                        (mutantType b)
  where isApp (splitTyConApp_maybe -> Just (con, _)) = isFunTyCon con
        isApp _                                      = False


mutantType (splitFunTy_maybe -> Just (a, b))
  = mkFunTy (mutantType a) (mutantType b)

mutantType (splitTyConApp_maybe -> Just (con, tys))
  = mkTyConApp (mutantTyCon con) (map mutantType tys)

mutantType (splitForAllTy_maybe -> Just (tyVar, ty))
  = mkForAllTy tyVar $ mkForAllTy (mutantTyVar tyVar) (mutantType ty)


mutantTyVar = mutantId

mutantTyCon tyCon = kind `seq` newTyCon
 where
  newTyCon | isAlgTyCon tyCon      = mutantAlgTyCon
           | isAbstractTyCon tyCon = makeTyConAbstract $ mutantAlgTyCon
           | isClassTyCon tyCon    = mutantClassTyCon
           | isFunTyCon tyCon      = mutantFunTyCon
           | isPrimTyCon tyCon     = tyCon
           | otherwise = trace ("Don't know how to mutate tyCon " ++ 
                                 (showSDoc.ppr.getName$tyCon) ++
                                  " :: " ++ (showSDoc.ppr$tyCon))
                               tyCon

  mutantClassTyCon = mkClassTyCon name kind tyvars rhs cls isRec
  mutantAlgTyCon = mkAlgTyCon name kind tyvars predTys rhs parent 
                              isRec hasGen declaredGadt
  mutantFunTyCon = tyCon -- mkFunTyCon (mutantName $ getName tyCon) (tyConKind tyCon)

  name = mutantName $ getName tyCon
  tyvars = interlace (tyConTyVars tyCon)
                     (map mutantTyVar $ tyConTyVars tyCon)
  rhs = mutantAlgTyConRhs tyCon newTyCon $ algTyConRhs tyCon
  cls = mutantClass (fromJust $ tyConClass_maybe tyCon)
  kind = duplicateKindArgs $ tyConKind tyCon
  isRec = boolToRecFlag $ isRecursiveTyCon tyCon
  predTys = tyConStupidTheta tyCon -- can we incrementalise constraints?
  parent = tyConParent tyCon
  hasGen = tyConHasGenerics tyCon
  declaredGadt = isGadtSyntaxTyCon tyCon

-- the idea here is that a kind of * -> * should become a kind of * -> * -> *, to make room
-- for the incrementalised versions of the type parameters.
duplicateKindArgs (splitTyConApp_maybe -> Just (tyCon, []))
  = mkTyConApp tyCon [] 
-- if we go from TyConApp to TyConApp, then we get a (-> * * *) rather than a (* -> * -> *).
-- Going from TyConApp to FunTy doesn't seem to have that problem.
duplicateKindArgs kind@(splitTyConApp_maybe -> Just (tyCon, kinds))
  = foldl (flip mkFunTy) (head argKinds) $ (tail argKinds) ++ argKinds ++ [resultKind]
  --foldr mkFunTy (resultKind) $ argKinds ++ argKinds ???
  where (resultKind:(reverse -> argKinds)) = reverse kinds
duplicateKindArgs (splitFunTy_maybe -> Just (arg, res))
  = mkFunTy arg res
duplicateKindArgs args = args

mutantAlgTyConRhs tyCon newTyCon (DataTyCon dataCons isEnum)
  = DataTyCon ((map mutantDataCon dataCons) ++ 
               (additionalMutantDataCons tyCon newTyCon))
              isEnum
mutantAlgTyConRhs _ _ (NewTyCon dataCon rhs etadRhs co)
  = NewTyCon (mutantDataCon dataCon)
             (mutantType rhs)
             (mutantEtadType etadRhs)
             (fmap mutantTyCon co)
mutantAlgTyConRhs _ _ AbstractTyCon
  = AbstractTyCon
mutantAlgTyConRhs _ _ DataFamilyTyCon
  = DataFamilyTyCon

mutantDataCon dataCon = mkDataCon 
  (mutantName $ dataConName dataCon)
  (dataConIsInfix dataCon)
  (dataConStrictMarks dataCon)
  (map mutantName $ dataConFieldLabels dataCon)
  (interlace (dataConUnivTyVars dataCon)
             (map mutantTyVar $ dataConUnivTyVars dataCon))
  (map mutantTyVar $ dataConExTyVars dataCon)
  (map2 mutantTyVar mutantType $ dataConEqSpec dataCon)
  (dataConDictTheta dataCon) -- dataConTheta on GHC 7.4?
  (map mutantType $ dataConOrigArgTys dataCon)
  (mutantType $ dataConOrigResTy dataCon)
  (mutantTyCon $ dataConTyCon dataCon)
  (dataConStupidTheta dataCon)
  (DCIds (fmap mutantId $ dataConWrapId_maybe dataCon)
         (mutantId $ dataConWorkId dataCon))

data AdditionalConType = AddConReplacement
                       | AddConHoist
                       | AddConIdentity
  deriving Show
additionalConTypes = [AddConReplacement, AddConHoist, AddConIdentity]
additionalConSuffix AddConReplacement = "replace"
additionalConSuffix AddConHoist       = "hoist"
additionalConSuffix AddConIdentity    = "identity"

additionalMutantDataCons oldTyCon newTyCon = map addAdditionalCon
                                                 additionalConTypes
  where addAdditionalCon type_ = additionalCon type_
        additionalCon = additionalMutantDataCon newTyCon oldTyCon
additionalMutantDataCon newTyCon oldTyCon addConType
  = let tyConName = getName newTyCon
        con = mkDataCon 
                (additionalMutantDataConName tyConName  addConType)
                False                       -- is infix?
                [HsNoBang | _ <- argTypes ] -- strictness annotations
                []                          -- field lables 
                (tyConTyVars newTyCon)      -- universally quantified type vars
                []                          -- existentially quantified type vars
                []                          -- gadt equalities
                []                          -- theta type
                argTypes                    -- original argument types
                (mkTyConTy newTyCon)-- ???  -- original result type
                newTyCon                    -- representation type constructor
                []                          -- stupid theta
                (mkDataConIds (additionalMutantDataConWrapId tyConName
                                                             addConType)
                              (additionalMutantDataConWorkId tyConName
                                                             addConType)
                              con)
        argTypes = additionalMutantDataConArgTypes oldTyCon addConType
     in  con
additionalMutantDataConName tyConName addConType
  = mutantNameIntoSpace tyConName 
                        OccName.dataName 
                        (additionalConSuffix addConType)
additionalMutantDataConWorkId tyConName addConType
  = mutantNameIntoSpace tyConName 
                        OccName.varName 
                        ("data_con_work_" ++ additionalConSuffix addConType)
additionalMutantDataConWrapId tyConName addConType
  = mutantNameIntoSpace tyConName 
                        OccName.varName 
                        ("data_con_wrap_" ++ additionalConSuffix addConType)
additionalMutantDataConReplaceVar tyCon
  = mkTyVar (mutantNameIntoSpace (getName tyCon)
                                 OccName.varName 
                                 "tyvar_for_replacement_value")
            liftedTypeKind
additionalMutantDataConArgTypes tyCon AddConReplacement
  = [funTyConToAppTy tyCon]
additionalMutantDataConArgTypes _ _ = [] 
  
funTyConToAppTy tyCon = mkAppTys (mkTyConTy tyCon) 
                                 (map mkTyVarTy $ tyConTyVars tyCon)

mutantEtadType (tyVars, type_) = (map mutantTyVar tyVars, mutantType type_)

mutantClass = id

exprVar (Var id) = id
exprVar other = error ("exprVar " ++ (showSDoc $ ppr other))

exprType :: Expr CoreBndr -> Type
exprType (Var id) = varType id
exprType (Lit lit) = literalType lit
exprType (App expr arg) = mkAppTy (exprType expr) (exprType arg)
exprType (Lam id expr) = exprType expr
exprType (Let bind expr) = exprType expr
exprType (Case expr id type_ alts) = type_
exprType (Cast expr coercion) = coercion
exprType (Type type_) = type_
exprType (Note note expr) = exprType expr

lookupDataCon type_ additionalCon
  = let tyCon = tyConAppTyCon type_
        cons | isAlgTyCon tyCon = data_cons $ algTyConRhs tyCon
             | otherwise = error $ "not an alg ty con" ++
                                   (showSDoc $ ppr tyCon)
        suffix = additionalConSuffix additionalCon
        nameString c = occNameString $ nameOccName $ dataConName c
        matchingCon c = List.isSuffixOf suffix (nameString c)
     in case filter matchingCon cons of
          (con:_) -> con
          []      -> error $ "Couldn't find " ++ show additionalCon ++
                             " for " ++ (showSDoc $ ppr $ type_)

{-
import Utils

incrementalize = mutant

mutant (Module name tdefs vdefgs) = Module name tdefs' vdefgs'
  where tdefs' = tdefs ++ (mutant_tdefs tdefs)
        vdefgs' = vdefgs ++ (mutant_vdefgs vdefgs)

mutant_tdefs = map mutant_tdef
mutant_tdef (Data qTcon tbinds cdefs) = Data (incrementalise_name qTcon) (tbinds ++ mutant_tbinds tbinds) $ 
                                             (mutant_cdefs cdefs) ++ (mutant_cdefs_builds qTcon tbinds cdefs) ++ [hoist_cdef qTcon, replacement_cdef qTcon tbinds]
mutant_tdef (Newtype qTcon1 qTcon2 tbinds ty) = Newtype (incrementalise_name qTcon1) (incrementalise_name qTcon2) (mutant_tbinds tbinds) (mutant_ty ty)

mutant_cdefs_builds qTcon tbinds cdefs = concat $ map (mutant_cdef_builds qTcon) cdefs
mutant_cdef_builds qTcon (Constr qDcon tbinds tys) = generate_cdef_builds qTcon tys builder
  where builder tys1 ty tys2 n = [Constr (apply_to_name (++ "_build_using_" ++ (show n)) $ incrementalise_name qDcon) tbinds (tys1 ++ tys2)]

mutant_tbinds = map mutant_tbind
mutant_tbind (tvar, kind) = (incrementalise_string tvar, kind)

mutant_cdefs = map mutant_cdef
mutant_cdef (Constr dcon tbinds tys) = Constr (incrementalise_name dcon) (mutant_tbinds tbinds) (mutant_tys tys)

hoist_cdef qTcon = Constr (hoistable_type_reference $ Tcon qTcon) [] []
replacement_cdef qTcon tbinds = Constr (replacement_type_reference $ Tcon qTcon) tbinds [(Tcon qTcon)]

mutant_tys = map mutant_ty
mutant_ty (Tvar tvar) = Tvar $ incrementalise_string tvar
mutant_ty (Tcon qTcon) = Tcon $ incrementalise_name qTcon
mutant_ty (Tapp ty1 ty2) = Tapp (Tapp (mutant_ty ty1) ty2) (mutant_ty ty2)
mutant_ty (Tforall tbind ty) = Tforall tbind (mutant_ty ty)
mutant_ty (TransCoercion ty1 ty2) = TransCoercion (mutant_ty ty1) (mutant_ty ty2)
mutant_ty (SymCoercion ty) = SymCoercion (mutant_ty ty)
mutant_ty (InstCoercion ty1 ty2) = InstCoercion (mutant_ty ty1) (mutant_ty ty2)
mutant_ty (LeftCoercion ty) = LeftCoercion (mutant_ty ty)
mutant_ty (RightCoercion ty) = RightCoercion (mutant_ty ty)
mutant_ty (UnsafeCoercion ty1 ty2) = UnsafeCoercion (mutant_ty ty1) (mutant_ty ty2)

mutant_vdefgs = map mutant_vdefg
mutant_vdefg (Rec vdefs) = Rec $ map mutant_vdef vdefs
mutant_vdefg (Nonrec vdef) = Nonrec $ mutant_vdef vdef

mutant_vdef (Vdef (qVar, ty, exp)) = Vdef (incrementalise_name qVar, mutant_ty ty, mutant_exp exp)

mutant_vbinds = map mutant_vbind
mutant_vbind (var, ty) = (zencode $ incrementalise_string var, mutant_ty ty)
mutant_bind (Vb vbind) = Vb $ mutant_vbind vbind
mutant_bind (Tb tbind) = Tb $ mutant_tbind tbind

mutant_alts ty result_ty alts = (mutant_alt_lits ty result_ty alts):(concat (map (mutant_alt ty result_ty) alts))
--mutant_alt ty (Acon qDcon tbinds [] exp) = [Acon (adjust_type_reference "replace" ty) (mutant_tbinds tbinds) [("a", Tvar "a")] (App (Dcon $ adjust_type_reference "replace" ty) exp)]
mutant_alt ty result_ty (Acon qDcon tbinds [] exp) = []
mutant_alt ty result_ty (Acon qDcon tbinds vbinds exp) = (builds [] vbinds 0) $ [recursive_case]
  where recursive_case = Acon (incrementalise_name qDcon) (mutant_tbinds tbinds) (mutant_vbinds vbinds) (mutant_exp exp)
        builds :: [Vbind] -> [Vbind] -> Int -> [Alt] -> [Alt]
        builds vbinds1 (vbind@(vbind_var,vbind_ty):vbinds2) n cons
          -- This is all about building a new value around an old one, for instance consing an element onto the head of a list.
          -- n is the index of the argument to the constructor that is being fed the old value, so in `data List a = Cons a (List a)' it would point to (List a)
          -- We require all other arguments to the constructor being incrementalised - in this case, just a - to be provided as part of the incrementalised_build constructor
          -- Consider a function length (x:xs) = 1 + length xs
          -- We need to turn this into something that will return an incrementalised value, where the "length xs" term has gone away.
          -- To achieve this, we incrementalise the RHS as usual, but we make some assignments beforehand. We define xs_incrementalised as a hoist value, and elsewhere arrange
          -- for length_incrementalised to respond to a hoist value by making the term go away. More work is required to make the right thing happen when
          -- more than one hoist value is involved.
          -- When we incrementalise the RHS, it will expect incrementalised values to exist for all the other terms in the expression (x, in this case, although it is not used in
          -- the expression). In order to make those terms available, we construct replace values for each argument provided as part of the incrementalised_build constructor and assign
          -- them to the incrementalised name of the argument.
          | ty == vbind_ty = let con = apply_to_name (++ "_build_using_" ++ (show n)) $ incrementalise_name qDcon
                                 acon = Acon con (mutant_tbinds tbinds) (vbinds1 ++ vbinds2) $ 
                                          Let hoist_defn $ 
                                            foldr (\replacement_vdefg exp -> Let replacement_vdefg exp) (mutant_exp exp) replacement_vdefgs
                                 name_to_hoist = vbind_var :: String
                                 names_to_replace = vbinds1 ++ vbinds2
                                 hoist_defn = Nonrec $ Vdef (incrementalise_name (Nothing, name_to_hoist), Tvar $ snd $ hoistable_type_reference vbind_ty, Var $ hoistable_type_reference vbind_ty)
                                 replacement_vdefgs = map replacement_vdefg names_to_replace
                                   where replacement_vdefg :: Vbind -> Vdefg
                                         replacement_vdefg (var, ty) = Nonrec $ Vdef (incrementalise_name (Nothing, var), mutant_ty $ snd vbind, 
                                                                                        App (Var $ replacement_type_reference ty) (Var (Nothing, var)) )
                
                                 -- new_exp = replace_exp replacement_exp (snd $ incrementalise_name $ (Nothing, name_to_replace)) (mutant_exp exp)
                                 -- replacement_exp = Var $ hoistable_type_reference vbind_ty
                              in builds (vbinds1 ++ [vbind]) vbinds2 (n+1) (acon:cons)
          | otherwise      = builds (vbinds1 ++ [vbind]) vbinds2 (n+1) cons
        builds _ _ _ cons = cons
mutant_alt ty result_ty (Alit lit exp) = []
mutant_alt ty result_ty (Adefault exp) = [Adefault $ mutant_exp exp]

mutant_alt_lits ty result_ty alts = Acon (replacement_type_reference ty) [] [("replace_val", ty)]
                               (Case (Var $ unqual "replace_val") ("replace_val", ty) (mutant_ty ty)
                                     (mapMaybe (mutant_alt_lit ty result_ty) alts)
                               )
mutant_alt_lit ty result_ty (Alit lit exp)                 = Just $ Alit lit                 (App (Var $ replacement_type_reference $ result_ty) exp)
mutant_alt_lit ty result_ty (Acon qDcon tbinds vbinds exp) = Just $ Acon qDcon tbinds vbinds (App (Var $ replacement_type_reference $ result_ty) exp)
mutant_alt_lit ty result_ty (Adefault exp)                 = Just $ Adefault                 (App (Var $ replacement_type_reference $ result_ty) exp)

hoistable_type_reference = adjust_type_reference "hoist"
identity_type_reference = adjust_type_reference "identity"
replacement_type_reference = adjust_type_reference "replace"


adjust_type_reference n (Tcon tcon) = apply_to_name (++ "_" ++ n) $ incrementalise_name tcon
adjust_type_reference n (Tapp ty1 ty2) = adjust_type_reference n ty1

adjust_type_reference n other = error $ show $ Data.toConstr other


mutant_exp (Var qVar) = Var $ incrementalise_name qVar
mutant_exp (Dcon qDcon) = Dcon $ incrementalise_name qDcon
mutant_exp (Lit lit) = Lit lit
mutant_exp (App exp1 exp2) = App (mutant_exp exp1) (mutant_exp exp2)
mutant_exp (Appt exp ty) = Appt (mutant_exp exp) (mutant_ty ty)
mutant_exp (Lam (Tb tbind) exp) = Lam (mutant_bind $ Tb tbind) (mutant_exp exp)


-- for \ a -> b, need to check if a is a (incrementalize_type a)_hoist. If so, produce a (incrementalize_type b)_identity.
mutant_exp (Lam (Vb vbind) exp) = case type_of_exp exp of
                                    Right param_exp_type -> Lam (Vb newBind) $ Case (Var $ unqual $ fst newBind) vbind (snd newBind) [
                                                                                 Acon (hoistable_type_reference $ snd vbind) [] [] 
                                                                                      (Var $ identity_type_reference $ param_exp_type),
                                                                                 Adefault (mutant_exp exp) ]
                                                                               where (Vb newBind) = mutant_bind $ Vb vbind
                                    Left e -> Lam (mutant_bind $ Vb vbind) (mutant_exp exp)
mutant_exp (Let vdefg exp) = Let (mutant_vdefg vdefg) (mutant_exp exp)
mutant_exp (Case exp vbind ty alts) = Case (mutant_exp exp) (mutant_vbind vbind) (mutant_ty ty) (mutant_alts (snd vbind) ty alts)
mutant_exp (Cast exp ty) = Cast (mutant_exp exp) (mutant_ty ty)
mutant_exp (Note string exp) = Note string $ mutant_exp exp
mutant_exp (External string ty) = error "mutant_exp don't know externals from infernos"

replace_exp :: Exp -> Var -> Exp -> Exp
replace_exp replacement name_to_replace var@(Var qVar)
  | name_to_replace == (snd qVar) = replacement
  | otherwise                     = var
replace_exp replacement name_to_replace (App exp1 exp2) = App (replace_exp replacement name_to_replace exp1) (replace_exp replacement name_to_replace exp2)
replace_exp replacement name_to_replace (Appt exp ty) = Appt (replace_exp replacement name_to_replace exp) ty
replace_exp replacement name_to_replace (Lam bind exp) = Lam bind (replace_exp replacement name_to_replace exp)
replace_exp replacement name_to_replace (Let vdefg exp) = Let vdefg (replace_exp replacement name_to_replace exp)
replace_exp replacement name_to_replace (Case exp vbind ty alts) = Case (replace_exp replacement name_to_replace exp) vbind ty alts
replace_exp replacement name_to_replace (Cast exp ty) = Cast (replace_exp replacement name_to_replace exp) ty
replace_exp replacement name_to_replace (Note string exp) = Note string $ replace_exp replacement name_to_replace exp
replace_exp _ _ exp = exp

type_of_exp (Lam (Vb vbind) exp)     = type_of_exp exp
type_of_exp (Lam (Tb tbind) exp)     = Right $ Tvar $ fst tbind
type_of_exp (Let vdefg exp)          = type_of_exp exp
type_of_exp (Case exp vbind ty alts) = Right $ ty
type_of_exp (Appt exp ty)            = Right $ ty
type_of_exp (App exp _)              = type_of_exp exp
type_of_exp (Cast exp ty)            = Right $ ty
type_of_exp (Note string exp)        = type_of_exp exp
type_of_exp exp                      = Left $ "Unknown type of exp" ++ (show $ Data.toConstr exp )  ++ " " ++ show exp

unsafe_type_of_exp a = case type_of_exp a of
                         Right a -> a
                         Left e -> error e


-}

process targetFile = do
  defaultErrorHandler defaultDynFlags $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      let dflags' = dopt_set (foldl xopt_set dflags
                                    [ Opt_Cpp
                                    , Opt_ImplicitPrelude
                                    , Opt_MagicHash])
                             Opt_EmitExternalCore
      setSessionDynFlags dflags'
      target <- guessTarget targetFile Nothing
      setTargets [target]
      load LoadAllTargets
      modSum <- getModSummary $ mkModuleName targetFile
      p <- parseModule modSum
      t <- typecheckModule p
      d <- desugarModule t
      let d' = mutantCoreModule (ms_mod modSum) d
      liftIO $ do
        putStrLn $ showSDoc $ ppr $ mg_binds $ dm_core_module d'
        putStrLn $ showSDocDebug $ ppr $ mg_binds $ dm_core_module d'
      liftIO $ do
        lintPrintAndFail d'

      setSessionDynFlags $ dflags' { hscOutName = targetFile ++ ".o"
                                   , extCoreName = targetFile ++ ".hcr"
                                   }
      (hscGenOutput hscOneShotCompiler) (dm_core_module d') modSum Nothing
      return ()

lintPrintAndFail desugaredModule = do 
  let bindings = mg_binds $ dm_core_module desugaredModule
  let (errors, warnings) = lintCoreBindings bindings
  when ((not $ isEmptyBag errors) || (not $ isEmptyBag warnings)) $ do
    putStrLn $ showSDoc $ pprMessageBag errors
    putStrLn $ showSDoc $ pprMessageBag warnings
    exitFailure

main = do
  args <- getArgs
  when (length args /= 1) $ do
    putStrLn $ "Usage: Incrementalizer <from.hs>"
    exitFailure
  let (from:[]) = args
  process from

showDataConDetails dataCon = (concat $ List.intersperse "\n" [
   ("NAME " ++ (showSDoc $ ppr $ dataConName dataCon))
  ,("  isInfix " ++ (showSDoc $ ppr $ dataConIsInfix dataCon))
  ,("  strictMarks " ++ (showSDoc $ ppr $ dataConStrictMarks dataCon))
  ,("  fieldLabels " ++ (showSDoc $ ppr $ dataConFieldLabels dataCon))
  ,("  univTyVars " ++ (showSDoc $ ppr $ dataConUnivTyVars dataCon))
  ,("  exTyVars " ++ (showSDoc $ ppr $ dataConExTyVars dataCon))
  ,("  eqSpec " ++ (showSDoc $ ppr $ dataConEqSpec dataCon))
  ,("  dictTheta " ++ (showSDoc $ ppr $ dataConDictTheta dataCon)) -- dataConTheta on GHC 7.4?
  ,("  origArgs " ++ (showSDoc $ ppr $ dataConOrigArgTys dataCon))
  ,("  origRes " ++ (showSDoc $ ppr $ dataConOrigResTy dataCon))
  ,("  tyCon " ++ (showSDoc $ ppr $ dataConTyCon dataCon))
  ,("  repType " ++ (showSDoc $ ppr $ dataConRepType dataCon))
  ,("  stupidTheta " ++ (showSDoc $ ppr $ dataConStupidTheta dataCon))])
