{-# LANGUAGE ViewPatterns, TupleSections, ScopedTypeVariables, DoRec #-}

module Main where

import Control.Monad
import qualified Data.List as List
import System.Environment (getArgs)
import System.Exit


import Bag
import GHC.Paths ( libdir )
import CoreLint
import CoreMonad
import DataCon
import DynFlags
import ErrUtils
import HscTypes hiding (lookupDataCon, lookupType)
import HscMain
import InstEnv (is_cls, is_dfun, is_tys, is_tcs, is_tvs)
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
import Modify
import Names
import IncrExps
import IncrTypes
import Tracer

import Dbise

mutantCoreModule = modifyCoreModule $ ModifyFuncs {
  modifyCoreBinds = mutantCoreBinds,
  modifyName = incrementaliseName,
  --lookupmodifyTyCon :: TyCon -> TypeLookupM TyCon
  --lookupmodifyTyCon tyCon
  --   = lookupTyThingName (tyConName tyCon) >>=
  --                         return . tyThingTyCon . fromJustNote "lookupmodifyTyCon"
  modifyLookupTyCon = lookupMutantTyCon,
  modifyId = mutantId,
  modifyDataCon = mutantDataCon,
  modifyTyCon = mutantTyCon,
  modifyClass = mutantClass
}

process targetFile modName = do
  --addWay WayProf
  defaultErrorHandler defaultDynFlags $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      let dflags_xopts = foldl xopt_set dflags
                                    [ Opt_Cpp
                                    , Opt_ImplicitPrelude
                                    , Opt_RankNTypes
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
      d1 <- mutantCoreModule (ms_mod modSum) (verifySingularVarDecsCoreModule d)
      d' <- dbiseCoreModule (ms_mod modSum) d1
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

