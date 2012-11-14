{-# LANGUAGE ScopedTypeVariables, ViewPatterns, DoRec #-}
module Main where

import Db
import Fauxlude
import Reduce
import Types

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (foldrM)
import System.Cmd
import System.Exit
import System.IO

import Safe

import Language.Core.Core
import Language.Core.Parser
import Language.Core.ParseGlue

import Database.HDBC (commit, disconnect)

envFromModules :: [Module] -> HeapM Env
envFromModules mods
  = do rec
         env <- builtinsEnv resultEnv
         resultEnv <- foldrM (envFromModule resultEnv) env mods
       return resultEnv

envFromModule :: Env -> Module -> Env -> HeapM Env
envFromModule resultEnv mod@(Module mname tdefs vdefs) env
  = do tenv <- foldrM envFromTdef env tdefs
       foldrM (envFromVdefg resultEnv) tenv vdefs

envFromTdef (Data tcon tbinds cdefs) env
  = foldrM envFromCdef env cdefs
envFromTdef (Newtype qTcon qTcon' tbinds ty) env
  = do v <- heapAdd $ Thunk envEmpty (CoreExp $ Dcon qTcon') []
       return $ envInsert qTcon' v env
envFromCdef (Constr qDcon tbinds tys) env
  = do v <- heapAdd $ Thunk envEmpty (CoreExp $ Dcon qDcon) []
       return $ envInsert qDcon v env

      

eval :: [Module] -> Env -> Id -> Id -> [HeapValue] -> HeapM HeapValue
eval mods env startMod startFun args = do
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
  putStrLn "Starting"
  ((env, [initial_arg, initial_result]), initial_heap) <- runHeap $ do {
    env <- envFromModules mods ;
    liftIO $ printEnv env ;
    arg <- eval mods env "B" "initialzustate" [];
    deepseq arg;
    result <- eval mods env "B" "appzustate" [arg];
    deepseq result;
    return (env, [arg, result])
  }

  putStrLn "MAINY initial state evaluated"
  putStrLn $ "arg " ++ showValue initial_heap (heapGet' initial_heap initial_arg)
  putStrLn $ "result " ++ showValue initial_heap (heapGet' initial_heap initial_result)
  db_initial_arg <- initDb env initial_heap (heapGet' initial_heap initial_arg)
  rawSystem "sh" ["-c", "echo 'select * from everything;' | sqlite3 dbfile.db"]
  putStrLn "MAINY db arg inited"
  conn <- connection
  db_initial_result <- addDb conn env initial_heap (heapGet' initial_heap initial_result)
  commit conn
  disconnect conn
  rawSystem "sh" ["-c", "echo 'select * from everything;' | sqlite3 dbfile.db"]
  putStrLn "MAINY db result inited"
  
  ([arg_change, result_change], change_heap) <- runHeap $ do {
    env <- envFromModules mods ;
    dogString <- heapAdd =<< unpackCString [StringValue "Dog"];
    dogDefString <- heapAdd =<< unpackCString [StringValue "Man's Best Friend"];

    db_initial_arg_value <- heapAdd $ DatabaseValue db_initial_arg ;

    new_input <- eval mods env "B" "NewDefinitionInput" [dogString, dogDefString];
    arg_change <- eval mods env "Inctime"
                                "BuiltinListzuincrementalisedzubuildzuusingzu1"
                                [new_input];
    result_change <- eval mods env "B"
                                   "appzustatezuincrementalised"
                                   [db_initial_arg_value
                                   ,arg_change];
    deepseq arg_change;
    liftIO $ putStrLn "MAINY input changes are cooked";
    deepseq result_change;
    return [arg_change, result_change]
  }

  putStrLn $ "MAINY change produced " ++ showValue change_heap (heapGet' change_heap result_change)

  applyChangeDb db_initial_arg change_heap arg_change
  applyChangeDb db_initial_result change_heap result_change
  {-
  (putStrLn . showValueHeap) =<< runHeap (do
    result <- heapAdd $ DatabaseValue db_initial_result
    whnf result
    heapGet result) 
  -}
  return ()

printEnv env = do
  forM_ (envKeys env) $ \k ->
    putStrLn $ "XX builtin " ++ show k
