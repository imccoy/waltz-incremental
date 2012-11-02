{-# LANGUAGE ScopedTypeVariables, ViewPatterns, DoRec #-}
module Main where

import Fauxlude
import Reduce
import Types

import Control.Monad (forM_)
import Data.Foldable (foldrM)
import System.IO
import System.Exit

import Safe

import Language.Core.Core
import Language.Core.Parser
import Language.Core.ParseGlue


envFromModules :: [Module] -> HeapM Env
envFromModules mods
  = do rec
         env <- builtinsEnv resultEnv
         resultEnv <- foldrM (envFromModule resultEnv) env mods
       return resultEnv

envFromModule :: Env -> Module -> Env -> HeapM Env
envFromModule resultEnv mod@(Module mname _ vdefs) env
  = foldrM (envFromVdefg resultEnv) env vdefs

eval :: [Module] -> Id -> Id -> [HeapValue] -> HeapM HeapValue
eval mods startMod startFun args = do
  env <- envFromModules mods
  heapAdd $ Thunk env (CoreExp varExp) args
  where mod = moduleById startMod mods
        varExp = Var (Just $ moduleMname mod , startFun)

moduleMname (Module mname _ _) = mname

moduleById id mods = headNote ("Couldn't find module with id " ++ id) $
                       [ mod | mod <- mods, moduleId mod == id ]
moduleId (Module (M (_, _, id)) _ _) = id

coreFileContents filename = do
  file <- openFile filename ReadMode
  contents <- hGetContents file
  --putStrLn $ unlines $ map (("XX " ++ filename) ++) $ lines contents
  case parse contents 0 of
    (FailP e) -> do putStrLn "HORRIBLY WRONG"
                    putStrLn e
                    exitFailure
    (OkP e) -> return e

writeFileContents filename content = do
  file <- openFile filename WriteMode
  hPutStr file $ content
  hClose file

main = do
  mods <- mapM coreFileContents ["B.hcr", "Inctime.hcr", "../dbrun/src/Funcs.hcr" ]
  --printBuiltins
  putStrLn "Starting"
  putStrLn $ showValueHeap $ runHeap (do
    arg <- eval mods "B" "initialzustate" []
    result <- eval mods "B" "appzustate" [arg]
    deepseq result
    heapGet result) 
  return ()

printBuiltins = do
  env <- return . fst $ runHeap $ builtinsEnv envEmpty
  forM_ (envKeys env) $ \k ->
    putStrLn $ "XX builtin " ++ show k
