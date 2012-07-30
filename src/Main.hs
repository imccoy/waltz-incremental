{-# LANGUAGE ViewPatterns, TupleSections, ScopedTypeVariables, DoRec #-}

module Main where

import Control.Monad
import qualified Data.List as List
import Safe
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

import Lookups
import Names
import IncrExps
import IncrTypes
import Tracer

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
  mg_exports' <- withTypeLookups tyEnv $ mutantAvailInfos (mg_exports mg)
  return $ mg { mg_binds = coreBinds ++ typeclassBinds
               , mg_types = tyEnv
               , mg_dir_imps  = mutantDeps (mg_dir_imps mg) mod
               , mg_exports = mg_exports'
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

mutantAvailInfos = liftM concat . mapM mutantAvailInfo
mutantAvailInfo i@(Avail name) = return [i, Avail (mutantName name)]
-- does this one need to include our wacky additional data cons?
mutantAvailInfo i@(AvailTC name names) = do
   tycon <- lookupTyThingName name >>= return . tyThingTyCon . (fromJustNote $
                                           "no mutant tycon for exports" ++
                                               nameString name)
   tycon' <- lookupMutantTyCon tycon
   let cons = map dataConName $ tyConDataCons tycon'
   return [i, AvailTC (mutantName name)
                      (cons ++ (map mutantName names))]


mutantTyThing :: TyThing -> TypeLookupM (TyThing, [CoreBind], [Instance])
mutantTyThing (AnId id) = mutantId id >>= return . (,[],[]) . AnId
mutantTyThing (ADataCon con) = mutantDataCon con 
                                 >>= return . (,[],[]) . ADataCon
mutantTyThing (ATyCon con) = mutantTyCon con
                                 >>= return . (\(c,bs,is) -> (ATyCon c, bs, is))
mutantTyThing (AClass cls) = return $ (AClass $ mutantClass cls, [],[])

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
        putStrLn $ showSDoc $ ppr $ mg_binds $ dm_core_module d'
        putStrLn $ ms_hspp_file modSum

      liftIO $ do
        lintPrintAndFail d'
                    

      setSessionDynFlags $ dflags' { hscOutName = targetFile ++ ".s"
                                   , extCoreName = targetFile ++ ".hcr"
                                   , outputFile = Just $ targetFile ++ ".o"}

      (hscGenOutput hscBatchCompiler) (dm_core_module d') modSum Nothing
      (cg, _) <- cgGutsFromModGuts $ dm_core_module d' -- $ verifySingularVarDecsCoreModule d'
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
     putStrLn $ (showSDoc $ ppr $ cg_binds core)
     putStrLn $ (showSDoc $ ppr core_binds)
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

