{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad
import qualified Data.List as List
import Data.Maybe
import Debug.Trace
import System.Environment (getArgs)
import System.Exit

import Outputable

import Bag
import BasicTypes 
import GHC.Paths ( libdir )
import CoreLint
import CoreMonad
import CoreSyn
import DataCon
import ErrUtils
import HscTypes hiding (lookupDataCon)
import HscMain
import IdInfo
import Literal
import MkId
import Module
import Name hiding (varName)
import OccName hiding (varName)
import qualified OccName as OccName
import StaticFlags
import Type
import TyCon
import Unique
import Var

import DynFlags

import GHC hiding (exprType)


interlace :: [a] -> [a] -> [a]
interlace [] [] = []
interlace (a:as) (b:bs) = a:b:(interlace as bs)

map2 :: (a -> c) -> (b -> d) -> [(a, b)] -> [(c, d)]
map2 f g = map (\(a, b) -> (f a, g b))

listWithout n list = let (before, after) = splitAt n list
                      in before ++ (tail after)





mutantCoreModule mod dm = dm { dm_core_module = mutantModGuts mod (dm_core_module dm) }
mutantModGuts mod mg = mg { mg_binds = mutantCoreBinds (mg_binds mg)
                          , mg_types = mutantTypeEnv (mg_types mg)
                          , mg_dir_imps  = mutantDeps (mg_dir_imps mg) mod
                          , mg_exports = mutantAvailInfos (mg_exports mg)
                          }

mutantDeps imps mod = extendModuleEnv imps mod [(mkModuleName "Radtime", False, noSrcSpan)]

mutantAvailInfos = concatMap mutantAvailInfo
mutantAvailInfo i@(Avail name) = [i, Avail (mutantName name)]
-- does this one need to include our wacky additional data cons?
mutantAvailInfo i@(AvailTC name names) = [i, AvailTC (mutantName name)
                                                     (map mutantName names)]


mutantCoreBinds binds = concat $ map mutantCoreBind binds

mutantCoreBind :: CoreBind -> [CoreBind]
mutantCoreBind corebind@(NonRec name exp) 
  = [corebind, NonRec (mutantCoreBndr name) (mutantExp exp)]
mutantCoreBind corebinds@(Rec name_exps)
  = [Rec $ name_exps ++ (map2 mutantCoreBndr mutantExp name_exps)]

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
               | isLocalVar var = local_mk    VanillaId n t vanillaIdInfo
        local_mk | isExportedId var  = mkExportedLocalVar
                 | otherwise         = mkLocalVar

mutantNameIntoSpace :: Name -> NameSpace -> String -> Name
mutantNameIntoSpace oldName nameSpace suffix
  | isInternalName oldName = mkInternalName unique occName (nameSrcSpan oldName)
  | isExternalName oldName = mkExternalName unique (adaptModule $ 
                                                      nameModule oldName)
                                            occName (nameSrcSpan oldName)
  | isWiredInName oldName  = mkSystemName unique occName 
  | otherwise              = mkSystemName unique occName 
  where oldNameString
          -- anonymous vars differ in the unique, but not the name.
          | n == "ds"              = "ds" ++ (show $ getUnique oldName)
          | n == "+"               = "plus"
          | List.isPrefixOf "$f" n = "typeclass_" ++ drop 2 n
          | otherwise              = n
          where n = occNameString $ nameOccName $ oldName
        nameString | oldNameString == "[]" = "BuiltinList_" ++ suffix
                   | otherwise             = oldNameString ++ "_" ++ suffix
        occName = mkOccName nameSpace nameString
        unique = getUnique occName
        adaptModule mod | modulePackageId mod == primPackageId = radtime
                        | modulePackageId mod == rtsPackageId  = radtime
                        | modulePackageId mod == basePackageId = radtime
                        | otherwise                            = mod
        radtime = mkModule mainPackageId (mkModuleName "Radtime") 

-- in The Real World, everything lives in a monad that lets you pluck new
-- uniques out of the unique supply. I didn't want to do that, so we
-- append a string according to the purpose that we want the unique for,
-- make the name with that string, and just use the unique from that new
-- name. Essentially we piggy-back on FastString's impure trickery.
mutantNameUnique oldName nameSpace suffix
  = setNameUnique oldName (nameUnique mutantName)
  where mutantName = mutantNameIntoSpace oldName nameSpace suffix

mutantName oldName = mutantNameIntoSpace oldName
                                         (occNameSpace $ nameOccName oldName)
                                         "incrementalised"

mutantExp (Var id) = Var $ mutantCoreBndr id
mutantExp (Lit lit) = Lit lit
mutantExp (App expr arg)
  | isTypeArg arg = App (App (mutantExp expr) arg) (mutantExp arg)
  | otherwise     = App (mutantExp expr) (mutantExp arg)
-- for \ a -> b, need to check if a is a (incrementalize_type a)_hoist. 
-- If so, produce a (incrementalize_type b)_identity.
mutantExp (Lam id expr) = expr'
  where expr' | isTyVar id = Lam id $ Lam (mutantCoreBndr id) (mutantExp expr)
              | otherwise  = Lam (mutantCoreBndr id)
                                 (Case (Var $ mutantCoreBndr id)
                                       (mutantCoreBndr id) 
                                       oType
                                       [def, hoist])
        def = (DEFAULT, [], mutantExp expr)
        hoist = (DataAlt $ lookupDataConByAdd iType AddConHoist
                ,[]
                ,dataConAtType (lookupDataConByAdd oType AddConIdentity)
                               oType
                )
        iType = mutantType $ varType id
        oType = exprType $ mutantExp expr
mutantExp (Let bind expr) = foldl (\e b -> Let b e) (mutantExp expr) (mutantCoreBind bind)
mutantExp c@(Case expr id type_ alts) = Case (mutantExp expr)
                                             (mutantCoreBndr id)
                                             (mutantType type_)
                                             ((mutantAlts alts) ++
                                              [replaceAlt c] ++
                                              builderAlts c)
mutantExp (Cast expr coercion) = Cast (mutantExp expr) (mutantType coercion)
mutantExp (Type type_) = Type (mutantType type_)
mutantExp (Note note expr) = Note note (mutantExp expr)

mutantAlts = mapMaybe mutantAlt 
mutantAlt (DataAlt _, [], _) = Nothing -- these cases get handled by replaceAlt
mutantAlt (DataAlt dataCon, binds, expr) = Just (DataAlt (mutantDataCon dataCon)
                                                ,map mutantId binds
                                                ,mutantExp expr)
mutantAlt (DEFAULT, [], expr) = Just (DEFAULT, [], mutantExp expr)
mutantAlt _ = Nothing -- if we handle changes-moving-into-a-value, then we should
                      -- probably do something for literals here
replaceAlt c@(Case expr id type_ alts)
  = (DataAlt $ lookupDataConByAdd (mutantType $ exprType expr) AddConReplacement
    ,[exprVar expr]
    ,App (dataConAtType con $ mutantType type_) c)
  where con = lookupDataConByAdd (mutantType type_) AddConReplacement

builderAlts (Case expr id type_ alts) = concatMap builderAlt alts

-- This is all about building a new value around an old one, for instance
-- consing an element onto the head of a list.  n is the index of the argument
-- to the constructor that is being fed the old value, so in `data List a =
-- Cons a (List a)' it would point to (List a) We require all other arguments
-- to the constructor being incrementalised - in this case, just a - to be
-- provided as part of the incrementalised_build constructor
--
-- Consider a function length (x:xs) = 1 + length xs We need to turn this into
-- something that will return an incrementalised value, where the "length xs"
-- term has gone away.  To achieve this, we incrementalise the RHS as usual,
-- but we make some assignments beforehand. We define xs_incrementalised as a
-- hoist value, and elsewhere arrange for length_incrementalised to respond to
-- a hoist value by making the term go away. More work is required to make the
-- right thing happen when more than one hoist value is involved.
--
-- When we incrementalise the RHS, it will expect incrementalised values to
-- exist for all the other terms in the expression (x, in this case, although
-- it is not used in the expression). In order to make those terms available,
-- we construct replace values for each argument provided as part of the
-- incrementalised_build constructor and assign them to the incrementalised
-- name of the argument.
builderAlt a@(DataAlt dataCon, binds, expr) = map (builderAlt' a) builders
  where builders = builderMutantDataConIndexes dataCon

builderAlt' (DataAlt dataCon, vars, expr) builderConIndex
  = (DataAlt builderCon
    ,builderArgs
    ,mkLets ((NonRec (mutantId replaceVar) (hoistValue replaceVar))
             :(map (\var -> NonRec (mutantId var) (replaceValue var))
                 builderArgs))
            (mutantExp expr))
  where builderCon = lookupDataConByBuilderIndex (mutantType $ type_)
                                                 builderConIndex
        type_ = dataConOrigResTy dataCon
        builderArgs = listWithout builderConIndex vars
        replaceVar = vars !! builderConIndex

replaceValue var = App (dataConAtType con $ mType) (Var var)
  where mType = mutantType $ varType var
        con = lookupDataConByAdd mType AddConReplacement

hoistValue var = dataConAtType con mType
  where con = lookupDataConByAdd mType AddConHoist
        mType = mutantType $ varType var

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

mutantTyCon tyCon = newTyCon
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
  = DataTyCon (map mutantDataCon dataCons ++ 
               additionalMutantDataCons tyCon newTyCon ++
               concatMap (builderMutantDataCons tyCon newTyCon) dataCons)
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

builderMutantDataCons oldTyCon newTyCon dataCon
  = map (builderMutantDataCon oldTyCon newTyCon dataCon) indexes
  where indexes = builderMutantDataConIndexes dataCon

builderMutantDataConIndexes dataCon = map fst index_types
  where all_index_types = zip [0..] (dataConOrigArgTys dataCon)
        index_types = filter (\(i, t) -> tyTyConMatches t tyCon)
                             all_index_types
        tyCon = dataConTyCon dataCon

tyTyConMatches (splitTyConApp_maybe -> Just (tyCon1, _)) tyCon2
  = tyCon1 == tyCon2
tyTyConMatches _ _ = False

builderMutantDataCon oldTyCon newTyCon dataCon n
  = let tyConName = getName newTyCon
        con = mkDataCon 
                (builderMutantDataConName tyConName n)
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
                (mkDataConIds (builderMutantDataConWrapId con n)
                              (builderMutantDataConWorkId con n)
                              con)
        argTypes = listWithout n (dataConOrigArgTys dataCon)
     in  con
                                                   
                                              
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
                (mkDataConIds (additionalMutantDataConWrapId con addConType)
                              (additionalMutantDataConWorkId con addConType)
                              con)
        argTypes = additionalMutantDataConArgTypes oldTyCon addConType
     in  con
additionalMutantDataConName tyConName addConType
  = mutantNameIntoSpace tyConName 
                        OccName.dataName 
                        (additionalConSuffix addConType)
additionalMutantDataConWorkId dataCon addConType
  = mutantNameUnique (dataConName dataCon)
                     OccName.varName 
                     ("data_con_work_" ++ additionalConSuffix addConType)
additionalMutantDataConWrapId dataCon addConType
  = mutantNameUnique (dataConName dataCon)
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

builderConSuffix n = "build_using_" ++ show n
builderMutantDataConName tyConName n
  = mutantNameIntoSpace tyConName 
                        OccName.dataName 
                        (builderConSuffix n)
builderMutantDataConWorkId dataCon n
  = mutantNameUnique (dataConName dataCon)
                     OccName.varName 
                     ("data_con_work_" ++ builderConSuffix n)
builderMutantDataConWrapId dataCon n
  = mutantNameUnique (dataConName dataCon)
                     OccName.varName 
                     ("data_con_wrap_" ++ builderConSuffix n)


  
funTyConToAppTy tyCon = mkAppTys (mkTyConTy tyCon) 
                                 (map mkTyVarTy $ tyConTyVars tyCon)

mutantEtadType (tyVars, type_) = (map mutantTyVar tyVars, mutantType type_)

mutantClass = id

exprVar (Var id) = id
exprVar other = error ("exprVar " ++ (showSDoc $ ppr other))

exprType :: Expr CoreBndr -> Type
exprType (Var id) = varType id
exprType (Lit lit) = literalType lit
exprType (App expr arg) = snd $ splitFunTys (exprType expr)
exprType (Lam id expr) = mkFunTy (varType id) (exprType expr)
exprType (Let bind expr) = exprType expr
exprType (Case expr id type_ alts) = type_
exprType (Cast expr coercion) = coercion
exprType (Type type_) = type_
exprType (Note note expr) = exprType expr

lookupDataCon type_ matches
  = let tyCon = case (splitTyConApp_maybe type_) of
                  Just (con, _) -> con
                  otherwise     -> error $ "so confused " ++ 
                                           (showSDoc $ ppr $ type_)
        cons | isAlgTyCon tyCon = data_cons $ algTyConRhs tyCon
             | otherwise = error $ "not an alg ty con" ++
                                   (showSDoc $ ppr tyCon)
        nameString c = occNameString $ nameOccName $ dataConName c
        matchingCon c = matches (nameString c)
     in case filter matchingCon cons of
          (con:_) -> Just con
          []      -> Nothing 
 



lookupDataConBySuffix type_ suffix
  = lookupDataCon type_ (List.isSuffixOf suffix)
 

lookupDataConByAdd type_ additionalCon
 = case lookupDataConBySuffix type_ (additionalConSuffix additionalCon) of
     Just a    -> a
     otherwise -> error $ "Couldn't find " ++ show additionalCon ++
                          " for " ++ (showSDoc $ ppr $ type_)
 
lookupDataConByBuilderIndex type_ builderIndex
 = case lookupDataConBySuffix type_ (builderConSuffix builderIndex) of
     Just a    -> a
     otherwise -> error $ "Couldn't find builder " ++ show builderIndex ++
                          " for " ++ (showSDoc $ ppr $ type_)

dataConAtType con type_ = foldl (\e t -> App e (Type t))
                                (Var $ dataConWorkId con)
                                typeArgs
  where typeArgs = case splitTyConApp_maybe type_ of
                     Just (_, args) -> args
                     otherwise      -> []


process targetFile moduleName = do
  defaultErrorHandler defaultDynFlags $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      let dflags_xopts = foldl xopt_set dflags
                                    [ Opt_Cpp
                                    , Opt_ImplicitPrelude
                                    , Opt_MagicHash]
      let dflags_dopts = foldl dopt_set dflags_xopts
                                    [Opt_EmitExternalCore
                                    , Opt_ForceRecomp]
                                    --, Opt_D_verbose_core2core]
      let dflags' = dflags_dopts -- { verbosity = 4 }
      liftIO $ addWay WayDebug
      setSessionDynFlags dflags'
      target <- guessTarget targetFile Nothing
      target_runtime <- guessTarget "Radtime" Nothing
      setTargets [target]
      liftIO $ putStrLn "loading."
      load LoadAllTargets
      liftIO $ putStrLn "loaded. Getting modSum."
      modGraph <- getModuleGraph
      liftIO $ putStrLn $ showSDoc $ ppr modGraph
      modSum <- getModSummary $ mkModuleName moduleName
      liftIO $ putStrLn "Got modSum. Parsing."
      p <- parseModule modSum
      t <- typecheckModule p
      d <- desugarModule t
      let d' = mutantCoreModule (ms_mod modSum) d
      liftIO $ do
        putStrLn $ showSDoc $ ppr $ mg_binds $ dm_core_module d'
        putStrLn $ ms_hspp_file modSum

      liftIO $ do
        lintPrintAndFail d'

      setSessionDynFlags $ dflags' { hscOutName = targetFile ++ ".s"
                                   , extCoreName = targetFile ++ ".hcr"
                                   , outputFile = Just $ targetFile ++ ".o"
                                   }
      (hscGenOutput hscBatchCompiler) (dm_core_module d') modSum Nothing
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
  when (length args > 2) $ do
    putStrLn $ "Usage: Incrementalizer from [moduleName]"
    exitFailure
  let (from, moduleName) = case args of
                             [from] -> (from, from)
                             [from, moduleName] -> (from, moduleName)
  process from moduleName

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
