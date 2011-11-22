import GHC
import Outputable
 
import GHC.Paths ( libdir )
--GHC.Paths is available via cabal install ghc-paths
import Bag
import MonadUtils
import Name
import OccName
import Type
import Unique

import Maybe

import DynFlags
targetFile = "B.hs"
 
-- findApplicationState lHsBinds = head $ bagToList $ filterBag matchApplicationState lHsBinds
--   where matchApplicationState lBind = case unLoc lBind of
--                                         FunBind id _ _ _ _ _ -> id == 

main = do
   res <- example
   putStrLn "done"
 
processBind bind = case (unLoc bind) of
                     (FunBind fun_id fun_infix (MatchGroup matches ty ) _ _ _) -> case occNameString $ nameOccName $ getName (unLoc fun_id) of
                                                                                    "main" -> liftIO $ do putStrLn $ "FunBind " ++ (showSDoc $ ppr fun_id)
                                                                                                          putStrLn $ "  " ++ (showSDoc $ ppr ty)
                                                                                    otherwise -> return ()
                     (PatBind pat_lhs pat_rhs _ _) -> liftIO $                                            putStrLn $ "PatBind " ++ (showSDoc $ ppr pat_lhs)
                     (VarBind var_id var_rhs var_inline) -> liftIO $                                      putStrLn $ "VarBind " ++ (showSDoc $ ppr var_id)
                     (AbsBinds abs_tvs abs_ev_vars abs_exports abs_ev_binds abs_binds) -> do mapBagM processBind abs_binds
                                                                                             return ()
--    (AbsBinds abs_tvs abs_ev_vars abs_exports abs_ev_binds abs_binds) -> liftIO $ putStrLn $ "AbsBind " ++ (case head abs_exports of 
--                                                                                                              (tyvars, id1, id2, tcSpecPrags) -> showSDoc $ ppr id1)

        -- mapBagM processBind s
        --
funBindingWithName s name = foldrBag bindingMatch Nothing s
  where bindingMatch _ (Just a) = Just a
        bindingMatch bind Nothing = case unLoc bind of
                                      bind@(FunBind fun_id _ _ _ _ _)   -> if (occNameString $ nameOccName $ getName $ unLoc fun_id) == name then Just bind else Nothing
                                      bind@(AbsBinds _ _ _ _ abs_binds) -> funBindingWithName abs_binds name
                                      otherwise                         -> Nothing

funBindingType fun_bind = t where (MatchGroup _ t) = fun_matches fun_bind

mutant fun_bind env args = mutant_expr expr env
                             where (MatchGroup gs _) = fun_matches fun_bind
                                   expr = case gs of
                                            (match:[]) -> expr 
                                              where 
                                               (Match lpats t grhss) = unLoc $ match
                                               (GRHS stmts expr) = unLoc $ head $ grhssGRHSs grhss
                                            otherwise -> error "polymorphic fns are not supported"


mutant_expr expr env = error "a"

example = 
    defaultErrorHandler defaultDynFlags $ do
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let dflags' = foldl xopt_set dflags
                            [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
        setSessionDynFlags dflags
        target <- guessTarget targetFile Nothing
        setTargets [target]
        load LoadAllTargets
        let appModuleName = mkModuleName "B"
        modSum <- getModSummary $ appModuleName
        p <- parseModule modSum
        t <- typecheckModule p
        d <- desugarModule t
        l <- loadModule d
        let s = typecheckedSource t
        let appStateFn = fromJust $ funBindingWithName s []
        mutant appStateFn ["toplevel"]
        let appStateFnType = funBindingType $ appStateFn
        liftIO $ do
          putStrLn $ showSDoc $ ppr $ appStateFn
        return s
