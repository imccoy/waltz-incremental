import GHC
import Outputable
 
import GHC.Paths ( libdir )
--GHC.Paths is available via cabal install ghc-paths
import Bag 
import MonadUtils

import DynFlags
targetFile = "B.hs"
 
-- findApplicationState lHsBinds = head $ bagToList $ filterBag matchApplicationState lHsBinds
--   where matchApplicationState lBind = case unLoc lBind of
--                                         FunBind id _ _ _ _ _ -> id == 

main = do
   res <- example
   putStrLn $ showSDoc ( ppr res )
 
processBind bind =
  case (unLoc bind) of
    (FunBind id infx (MatchGroup matches ty ) _ _ _) -> liftIO $ putStrLn $ showSDoc $ ppr ty
    otherwise -> liftIO $ putStrLn "skipping something"

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
        modSum <- getModSummary $ mkModuleName "B"
        p <- parseModule modSum
        t <- typecheckModule p
        d <- desugarModule t
        let s = typecheckedSource d
        mapBagM processBind s
        return s
