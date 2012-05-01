{-# LANGUAGE ViewPatterns, TupleSections, ScopedTypeVariables, DoRec #-}

module Main where

import Control.Monad
import qualified Data.List as List
import Data.Maybe
import System.Environment (getArgs)
import System.Exit


import Bag
import GHC.Paths ( libdir )
import CoreLint
import CoreMonad
import CoreSyn
import DataCon
import DynFlags
import ErrUtils
import HscTypes hiding (lookupDataCon, lookupType)
import HscMain
import IfaceSyn (IfaceInst (..))
import IfaceType (toIfaceTyCon_name)
import InstEnv (extendInstEnvList,
                is_cls, is_dfun, is_tys, is_tcs, is_tvs, is_flag)
import Module
import Outputable
import Type
import TysWiredIn
import UniqFM
import Var

import GHC hiding (exprType)

import CorePrep (corePrepPgm)
import CoreToStg (coreToStg)
import SimplStg (stg2stg)
import TidyPgm (tidyProgram)
import qualified Generator.TopLevel as Js (generate)
import Javascript.Language (Javascript)
import qualified Javascript.Formatted as Js

import AdditionalDataCons
import Lookups
import Names
import IncrTypes
import Tracer
import Utils

mutantCoreModule mod dm = do
  coreMod <- mutantModGuts mod (dm_core_module dm)
  return $ dm { dm_core_module = coreMod }

mutantModGuts mod mg = do
  (tyEnv, typeclassBinds, instances) <- mutantTypeEnv (mg_types mg)
  coreBinds <- withTypeLookups tyEnv $ mutantCoreBinds (mg_binds mg)
  let newIfaceInsts = map (\inst -> IfaceInst {
                              ifDFun = varName $ is_dfun inst
                             ,ifOFlag = is_flag inst
                             ,ifInstCls = is_cls inst
                             ,ifInstTys = map (fmap toIfaceTyCon_name)
                                              (is_tcs inst)
                             ,ifInstOrph = Nothing -- should be safe, since all
                                                   -- instances we generate are
                                                   -- for types defined in this
                                                   -- module
                            }
                          )
                          instances
  return $ mg { mg_binds = coreBinds ++ typeclassBinds
               , mg_types = tyEnv
               , mg_dir_imps  = mutantDeps (mg_dir_imps mg) mod
               , mg_exports = mutantAvailInfos (mg_exports mg)
               , mg_inst_env = extendInstEnvList (mg_inst_env mg) instances
               , mg_insts = instances
               }

mutantTypeEnv env = do
  rec { (result, classBinds, instances) <- withTypeLookups result $ do
          elt_classBinds_instances <- mapM mutantTyThing $ typeEnvElts env
          let elts       = map       (\(a,_,_) -> a) elt_classBinds_instances
          let classBinds = concatMap (\(_,b,_) -> b) elt_classBinds_instances
          let instances  = concatMap (\(_,_,c) -> c) elt_classBinds_instances
          let newElts = elts ++ map AnId (bindersOfBinds classBinds)
          return (extendTypeEnvList env newElts, classBinds, instances)
      }
  return (result, classBinds, instances)

mutantDeps imps mod = extendModuleEnv imps mod [(inctimeName, False, noSrcSpan)]

mutantAvailInfos = concatMap mutantAvailInfo
mutantAvailInfo i@(Avail name) = [i, Avail (mutantName name)]
-- does this one need to include our wacky additional data cons?
mutantAvailInfo i@(AvailTC name names) = [i, AvailTC (mutantName name)
                                                     (map mutantName names)]


mutantTyThing :: TyThing -> TypeLookupM (TyThing, [CoreBind], [Instance])
mutantTyThing (AnId id) = mutantId id >>= return . (,[],[]) . AnId
mutantTyThing (ADataCon con) = mutantDataCon con 
                                 >>= return . (,[],[]) . ADataCon
mutantTyThing (ATyCon con) = mutantTyCon con
                                 >>= return . (\(c,bs,is) -> (ATyCon c, bs, is))
mutantTyThing (AClass cls) = return $ (AClass $ mutantClass cls, [],[])

mutantCoreBinds = mapM mutantCoreBind

mutantCoreBind corebind@(NonRec name exp) = do
  name' <- mutantCoreBndr name
  exp' <- mutantExp exp
  return $ Rec [(name,exp), (name', exp')]
mutantCoreBind corebinds@(Rec name_exps) = do
  newNameExps <- map2M mutantCoreBndr mutantExp name_exps
  return $ Rec $ name_exps ++ newNameExps

mutantCoreBndr = mutantId

mutantExp :: Expr CoreBndr -> TypeLookupM (Expr CoreBndr)
mutantExp (Var id) = liftM Var $ lookupOrMutantId id
mutantExp (App expr arg)
  | isTypeArg arg = do expr' <- mutantExp expr
                       arg' <- mutantExp arg
                       argD <- expIncrementalisedDictionary arg
                       return $ App (App (App expr' arg) arg') argD
                       --return $ App (App expr' arg) arg'
  | otherwise     = liftM2 App (mutantExp expr) (mutantExp arg)
-- for \ a -> b, need to check if a is a (incrementalize_type a)_hoist. 
-- If so, produce a (incrementalize_type b)_identity.
mutantExp (Lam id expr) = do 
  expr' <- mutantExp expr
  id' <- lookupOrMutantId id
  idD <- varIncrementalisedDictionary incrementalisedDictionaryType
                                      id
                                      (typeFor id)
                                      (typeFor id')
  let iType = varType id'
  let oType = exprType expr'

  test <- liftM2 mkApps (liftM Var incrementalisedHoistTest)
                        (return $ [Type (varType id)
                                  , Type iType
                                  , Var idD
                                  , Var id'])
  idDvalue <- expIncrementalisedDictionary $ Type $ varType id
  let lets = [NonRec idD idDvalue]
                

  let def = (DEFAULT, [], expr')

  hoist <- do
    mk <- incrementalisedIdentityMk
    dict <- expIncrementalisedDictionary $ Type $ exprType expr
    return (DataAlt trueDataCon
              ,[]
              ,mkApps (Var mk)
                      [Type (exprType expr)
                      , Type oType
                      , dict]
              )
  let result | isTyVar id = Lam id $ Lam id' $ Lam idD expr'
             | otherwise  = Lam id' $ mkLets lets $
                                (Case test
                                      (testArgVar (nameString $ varName id')
                                                  (mkTyConTy boolTyCon)
                                                  0)
                                      oType
                                      [def, hoist])

  return result

mutantExp (Let bind expr) = liftM2 Let
                                   (mutantCoreBind bind)
                                   (mutantExp expr)
mutantExp c@(Case expr id type_ alts) = liftM4 Case 
                                                (mutantExp expr)
                                                (mutantCoreBndr id)
                                                (mutantType type_)
                                                (introduceSpecialAltCases c =<<
                                                 mutantAlts alts)
mutantExp (Cast expr coercion) = liftM2 Cast
                                         (mutantExp expr)
                                         (mutantType coercion)
mutantExp (Type type_) = liftM Type (mutantType type_)
mutantExp (Note note expr) = liftM (Note note) (mutantExp expr)
mutantExp (Lit lit) = return (Lit lit)

mutantAlts = liftM catMaybes . mapM mutantAlt

mutantAlt :: Alt CoreBndr -> TypeLookupM (Maybe (Alt CoreBndr))
mutantAlt (DataAlt _, [], _) = return Nothing -- handled by replaceAlt
mutantAlt ((DataAlt dataCon), binds, expr) = do
  dataCon' <- lookupMutantDataCon dataCon
  binds' <- mapM mutantId binds
  expr' <- mutantExp expr
  return $ Just $ ((DataAlt dataCon'), binds', expr')
mutantAlt (DEFAULT, [], expr) = do
  expr' <- mutantExp expr
  return $ Just (DEFAULT, [], expr')
mutantAlt _ = return Nothing -- if we handle changes-moving-into-a-value, then
                             -- we should probably do something  here

introduceSpecialAltCases c alts = do
  let (defaultAlts, nonDefaultAlts)
        = List.partition (\(a, _, _) -> a == DEFAULT) alts
  r <- replaceAlt' c defaultAlts
  b <- builderAlts' c r
  return $ [(DEFAULT, [], b)] ++ nonDefaultAlts


replaceAlt' c@(Case expr id type_ alts) defaultAlts = do
  destType <- mutantType type_
  srcType  <- mutantType $ exprType expr
  expr' <- mutantExp expr
  dict <- expIncrementalisedDictionary (Type $ exprType expr)

  replaceTest <- do
    t <- incrementalisedReplaceTest
    return $ mkApps (Var t) [Type $ exprType expr
                            ,Type srcType
                            ,dict]
  replaceExtractor <- do
    t <- incrementalisedReplaceExtractor
    return $ mkApps (Var t) [Type $ exprType expr
                            ,Type srcType
                            ,dict]
  replaceExp <- do
    mkReplace <- incrementalisedReplaceMk
    destDict <- expIncrementalisedDictionary (Type type_)
    return $ Let (NonRec (exprVar expr) (App replaceExtractor expr')) $
               mkApps (Var mkReplace)
                      [Type type_
                      ,Type destType
                      ,destDict
                      ,c]
  return $ Case (App replaceTest expr')
                (testArgVar "replaceAlt" (mkTyConTy boolTyCon) 0)
                destType
                (defaultAlts ++
                [(DataAlt trueDataCon
                 ,[]
                 ,replaceExp)]
                )


builderAlts' c@(Case expr id type_ alts) def
  = foldM (builderAlt' c) def alts

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
builderAlt' c def a@(DataAlt dataCon, binds, expr)
  = foldM (builderAltAtIndex' c a) 
          def
          (builderMutantDataConIndexes dataCon)
builderAlt' c def a = return def

builderAltAtIndex' (Case c_expr c_id c_type c_alts)
                   (DataAlt dataCon, vars, expr)
                   def
                   builderConIndex = do
  type_' <- mutantType type_
  let builderCon = lookupDataConByBuilderIndex type_'
                                               builderConIndex
  replaceVarId <- mutantId replaceVar
  replaceVarValue <- hoistValue replaceVar
  builderArgsIds <- mapM mutantId builderArgs
  builderArgsValues <- mapM replaceValue builderArgs
  expr' <- mutantExp expr
  let alt = (DataAlt builderCon
            ,builderArgs
            ,mkLets ((NonRec replaceVarId replaceVarValue)
                     :(map (\(id, val) -> NonRec id val)
                           (zip builderArgsIds builderArgsValues)))
                    expr')
  c_id' <- do
    var <- mutantCoreBndr c_id
    let suffix = "builder" ++ show builderConIndex
    let name = mutantNameUniqueLocal (varName var) suffix
    return $ setVarName var name

  liftM4 Case (mutantExp c_expr)
              (return c_id')
              (mutantType c_type)
              (return [(DEFAULT, [], def),alt])
  where type_ = dataConOrigResTy dataCon
        builderArgs = listWithout builderConIndex vars
        replaceVar = vars !! builderConIndex
        hoistValue var = incrementalisedDictElement 
                           incrementalisedHoistMk
                           (typeFor var)
        replaceValue var = liftM2 App
                                  (incrementalisedDictElement
                                     incrementalisedReplaceMk
                                     (typeFor var))
                                  (return $ Var var)




mutantClass = id

exprVar (Var id) = id
exprVar other = error ("exprVar " ++ (showSDoc $ ppr other))

typeFor :: Var -> Type
typeFor v
 | isTyVar v = mkTyVarTy v
 | otherwise = varType v

process targetFile modName = do
  --addWay WayProf
  defaultErrorHandler defaultDynFlags $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      let dflags_xopts = foldl xopt_set dflags
                                    [ Opt_Cpp
                                    , Opt_ImplicitPrelude
                                    , Opt_MagicHash]
      let dflags_dopts = foldl dopt_set dflags_xopts
                                    [Opt_EmitExternalCore
                                    , Opt_ForceRecomp
                                    , Opt_D_dump_hi]
                                    --, Opt_D_verbose_core2core]
      let dflags' = dflags_dopts -- { verbosity = 4 }
      setSessionDynFlags dflags'
      target <- guessTarget targetFile Nothing
      target_runtime <- guessTarget "Inctime" Nothing
      setTargets [target, target_runtime]
      liftIO $ putStrLn "loading."
      load LoadAllTargets
      liftIO $ putStrLn "loaded. Getting modSum."
      modGraph <- getModuleGraph
      liftIO $ putStrLn $ showSDoc $ ppr modGraph
      modSum <- getModSummary $ mkModuleName modName
      liftIO $ putStrLn "Got modSum. Parsing."
      p <- parseModule modSum
      t <- typecheckModule p
      d <- desugarModule t
      d' <- mutantCoreModule (ms_mod modSum) (verifySingularVarDecsCoreModule d)
                 -- >>= traceCoreModuleG
      liftIO $ do
        showAllModuleContents $ mg_types $ dm_core_module $ d'
        putStrLn $ showSDocDebug $ ppr $ mg_binds $ dm_core_module d'
        putStrLn $ ms_hspp_file modSum

      liftIO $ do
        lintPrintAndFail d'
                    

      setSessionDynFlags $ dflags' { hscOutName = targetFile ++ ".s"
                                   , extCoreName = targetFile ++ ".hcr"
                                   , outputFile = Just $ targetFile ++ ".o"}

      (hscGenOutput hscBatchCompiler) (dm_core_module d') modSum Nothing
      (cg, _) <- cgGutsFromModGuts $ dm_core_module $ verifySingularVarDecsCoreModule d'
      liftIO $ do
        js <- concreteJavascriptFromCgGuts dflags' $ cg
        writeFile (targetFile ++ ".js") js
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

-- this function stolen pretty hard from ghcjs
concreteJavascriptFromCgGuts :: DynFlags -> CgGuts -> IO String
concreteJavascriptFromCgGuts dflags core =
  do core_binds <- corePrepPgm dflags (cg_binds core) (cg_tycons $ core)
     stg <- coreToStg (modulePackageId . cg_module $ core) core_binds
     (stg', _ccs) <- stg2stg dflags (cg_module core) stg
     let abstract :: Javascript js => js
         abstract = Js.generate (cg_module core) stg'
     return $ show (abstract :: Js.Formatted)

-- this one stolen from the same place
cgGutsFromModGuts :: GhcMonad m => ModGuts -> m (CgGuts, ModDetails)
cgGutsFromModGuts guts =
  do hscEnv <- getSession
     simplGuts <- hscSimplify guts
     liftIO $ tidyProgram hscEnv simplGuts


showAllModulesContents :: Ghc ()
showAllModulesContents = do
  getSession >>= return . eltsUFM . hsc_HPT >>= mapM (showAllModuleContents . 
                                                      md_types . 
                                                      hm_details)
  getSession >>= 
    liftIO . hscEPS >>=
    return . eps_PTE >>= 
    showAllModuleContents

showAllModuleContents mod = do
  liftIO $ putStrLn $ showSDoc $ ppr $ eltsUFM mod

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

printAllInsts insts = forM insts $ \inst -> do
  putStrLn $ "Class is " ++ (nameString $ is_cls inst)
  putStrLn $ "Type is " ++ (showSDoc $ ppr $ varType $ is_dfun inst)
  putStrLn $ "Tys are" ++ (showSDoc $ ppr $ is_tys inst)
  putStrLn $ "RoughMatch tcs are " ++ (showSDoc $ ppr $ is_tcs inst)
  putStrLn $ "Tvs are " ++ (showSDoc $ ppr $ is_tvs inst)

