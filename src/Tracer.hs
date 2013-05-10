{-# LANGUAGE ViewPatterns #-}
module Tracer where

import Control.Monad
import Data.List (find)
import Data.Maybe

import CoreSyn (Expr (..), Bind (..), CoreBind, CoreBndr (..), Note (..),
                mkStringLit, isTypeArg, bindersOfBinds, bindersOf)
import GHC (dm_core_module, DesugaredModule)
import HscTypes (ModGuts (..), tyThingId)
import MkCore (mkCoreApps)
import Name (nameModule)
import Outputable (showSDoc, showSDocDebug, ppr)
import Type
import Var (Var, varType)

import ExprUtils
import Lookups
import PrelNames (unpackCStringName)

traceCoreModuleG a = withTypeLookupsDefault $ traceCoreModule a

traceCoreModule dm = do
  coreMod <- traceModGuts (dm_core_module dm)
  return $ dm { dm_core_module = coreMod}

traceModGuts mg = do
  coreBinds <- mapM traceCoreBind $ mg_binds mg
  return $ mg { mg_binds = coreBinds }

traceCoreBind :: CoreBind -> TypeLookupM CoreBind
traceCoreBind corebind@(NonRec name exp) = do
  liftM (NonRec name) (traceTopLevelExp name exp)

traceCoreBind corebind@(Rec name_exps) = do
  liftM Rec $ forM name_exps $ \(name, exp) -> do
    exp' <- traceTopLevelExp name exp
    return (name, exp')

mkTrace (isForAllTy -> True) _ _ e = return e
mkTrace t n s e = mkTraceS t ((showSDoc $ ppr n) ++ ": " ++ s) e

mkTraceS t s e = do
  traceFn <- lookupPreludeFn "Debug.Trace" "trace"
  stringUnpack <- lookupTypeM (Just $ nameModule unpackCStringName)
                              (lookupNameEnv unpackCStringName)
  let mkStringExpr a = App (Var $ tyThingId $ fromJust stringUnpack)
                           (mkStringLit s)
  return $ mkCoreApps (Var traceFn) [Type t, mkStringExpr s, e]

traceTopLevelExp name exp = mkTraceS (varType name)
                                     ("Entering " ++ (showSDoc $ ppr name))
                                     =<< traceExp name exp

traceExp name e@(Var id) =
  mkTrace (varType id)
          name
          ("Var " ++ (showSDoc $ ppr id))
          e
traceExp name e@(App expr (Type arg)) = do
  let (expr0, args) = dig e []
  expr' <- traceExp name expr0
  mkTrace (exprType e)
          name
          ("App (" ++ (showSDoc $ ppr expr) ++ ") " ++
                "(" ++ (showSDoc $ ppr arg) ++ ")") $
          Note (CoreNote $ "From " ++ (showSDoc $ ppr e) ++ 
                           " got args " ++ (showSDoc $ ppr args))
               (mkCoreApps expr' args)
  where dig (App expr t@(isTypeArg -> True)) ts = dig expr (t:ts)
        dig expr ts = (expr, ts)
traceExp name e@(App expr arg) = do
  arg' <- traceExp name arg
  expr' <- traceExp name expr
  mkTrace (exprType e)
          name
          ("App (" ++ (showSDoc $ ppr expr) ++ ") " ++
                "(" ++ (showSDoc $ ppr arg) ++ ")")
          (App expr' arg')
traceExp name e@(Lam id expr) = 
  liftM (Lam id) $ traceExp name expr
traceExp name e@(Let bind expr) = do
  liftM2 Let (traceCoreBind bind) (traceExp name expr)
traceExp name e@(Case expr id type_ alts) = do
  expr' <- traceExp name expr
  alts' <- forM alts $ \(alt,args,alt_exp) -> do
             alt_exp' <- mkTrace type_
                                 name
                                 ("Case alt: " ++ (showSDoc $ ppr alt))
                             =<< traceExp name alt_exp
             return (alt, args, alt_exp')
  mkTrace type_
          name
          ("Case scrutinee: " ++ (showSDoc $ ppr expr))
          (Case expr' id type_ alts')
traceExp name e@(Cast expr coercion) = 
  liftM2 Cast
         (traceExp name expr)
         (return coercion)
traceExp name e@(Type type_) = return e
traceExp name e@(Note note expr) = traceExp name expr
traceExp name e@(Lit lit) = return e

 
verifySingularVarDecsCoreModule :: DesugaredModule -> DesugaredModule
verifySingularVarDecsCoreModule dm =
  let coreMod = verifySingularVarDecsModGuts (dm_core_module dm)
   in dm { dm_core_module = coreMod}

verifySingularVarDecsModGuts mg =
  let names = concat $ map (dupVarDecsCoreBind [] "" (mg_binds mg)) $ 
                           mg_binds mg
   in if names == []
        then mg
        else error $ "Duplicate names " ++ concatMap fmt names
  where fmt (p1, p2, n) = concat ["(", p1, ", ",
                                       p2, ", ",
                                       showSDocDebug $ ppr n, ")"]

withName names name l = case find (\a -> snd a == snd name) names of
                          Just (p, _) -> (p, fst name, snd name):l
                          otherwise   -> l 

withPath p ns = zip (repeat p) ns
addPath p name = addPathS p (showSDoc $ ppr name)
addPathS p s = p ++ "/" ++ s

dupVarDecsCoreBind :: [(String, Var)] -> 
                      String -> 
                      [CoreBind] -> 
                      CoreBind -> 
                      [(String, String, Var)]
dupVarDecsCoreBind names path binds corebind@(NonRec name exp) =
  withName names (path, name) $ dupVarDecsExp (names ++ 
                                               withPath path
                                                        (bindersOfBinds binds))
                                              (path ++ addPath path name)
                                              exp

dupVarDecsCoreBind names path binds corebind@(Rec name_exps) = do
  concat $ map (\(name, exp) -> withName names (path, name) $
                                  dupVarDecsExp (withPath path
                                                         (bindersOfBinds binds) 
                                                 ++ names)
                                                (path ++ addPath path name)
                                                exp)
               name_exps

dupVarDecsExp :: [(String, Var)] -> 
                 String ->
                 Expr CoreBndr ->
                 [(String, String, Var)]
dupVarDecsExp names path e@(Var id) = []

dupVarDecsExp names path e@(App expr arg) =
  dupVarDecsExp names path expr ++ dupVarDecsExp names path arg

dupVarDecsExp names path e@(Lam id expr) = 
  withName names (path,id) $ (dupVarDecsExp ((path,id):names) path expr)

dupVarDecsExp names path e@(Let bind expr) =
  dupVarDecsCoreBind names (addPathS pathL "bind") [bind] bind ++
  dupVarDecsExp (names ++ withPath pathE (bindersOf bind)) pathE expr
  where pathL = addPathS path "Let"
        pathE = addPathS pathL "exp"

dupVarDecsExp names path e@(Case expr id type_ alts) =
  withName names (path,id) $ scrutinee_dups ++ alts_dups 
  where scrutinee_dups = dupVarDecsExp ((addPathS pathC "Scrutinee",id):names)
                                       pathC
                                       expr 
        alts_dups = concatMap alt_dups alts
        alt_dups (alt,args,alt_exp) =
          let pathA = addPathS pathC $ 
                               "Alt " ++ (showSDoc $ ppr alt)
              args' = map (\(arg, index) -> 
                             (addPathS pathA ("Arg " ++ show index), arg))
                          (zip args [0..])
           in dupVarDecsExp ((pathC,id):(args' ++ names))
                            (addPathS pathA "Exp")
                            alt_exp
        pathC = addPathS path $ "Case " ++ (showSDocDebug $ ppr id)

dupVarDecsExp names path e@(Cast expr coercion) = 
  dupVarDecsExp names path expr

dupVarDecsExp names path e@(Type type_) = []

dupVarDecsExp names path e@(Note note expr) = 
  dupVarDecsExp names path expr

dupVarDecsExp names path e@(Lit lit) = []
