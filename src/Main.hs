{-# LANGUAGE ScopedTypeVariables, ViewPatterns, DoRec #-}
module Main where

import Fauxlude
import Types

import Data.Foldable (foldrM)
import System.IO
import System.Exit

import Safe

import Language.Core.Core
import Language.Core.Parser
import Language.Core.ParseGlue

envFromModules :: [Module] -> HeapM Env
envFromModules mods
  = do env <- builtinsEnv
       rec { resultEnv <- foldrM (envFromModule resultEnv) env mods }
       return resultEnv

envFromModule :: Env -> Module -> Env -> HeapM Env
envFromModule resultEnv mod@(Module mname _ vdefs) env
  = foldrM (envFromVdefg resultEnv) env vdefs

envFromVdefg :: Env -> Vdefg -> Env -> HeapM Env
envFromVdefg resultEnv vdefg env
  = case vdefg of
      (Rec vdefs)   -> foldrM envFromVdef env vdefs
      (Nonrec vdef) -> envFromVdef vdef env
  where envFromVdef (Vdef (qVar, ty, exp)) env
           = do v <- heapAdd $ Thunk resultEnv (CoreExp exp) []
                return $ envInsert qVar v env

eval :: [Module] -> Id -> Id -> [HeapValue] -> HeapM HeapValue
eval mods startMod startFun args = do
  env <- envFromModules mods
  heapAdd $ Thunk env (CoreExp varExp) args
  where mod = moduleById startMod mods
        varExp = Var (Just $ moduleMname mod , startFun)

whnf :: HeapValue -> HeapM ()
whnf heapValue = go =<< heapGet heapValue
  where go value | isThunkValue value = reduce heapValue >> whnf heapValue
                 | otherwise          = return ()

deepseq :: HeapValue -> HeapM ()
deepseq heapValue = do go =<< (whnf heapValue >> heapGet heapValue)
  where go (DataValue con args) = mapM_ deepseq args
        go v                    = return ()

reduce :: HeapValue -> HeapM ()
reduce heapValue = heapChange (reduce' heapValue) heapValue

reduce' heapValue v@(isThunkValue -> False)                 = return v
reduce' heapValue thunk@(Thunk { thunkExp = (RtExp f _ a) })= do
  mapM_ whnf $ thunkArgs thunk
  case compare a (length (thunkArgs thunk)) of
    LT -> do let (a1, a2) = splitAt a (thunkArgs thunk)-- oversaturated
             addThunkArgs a2 =<< f a1
    EQ -> do f $ thunkArgs thunk
    GT -> do h <- heapGetFull
             error ("PAP (got " ++ show (length $ thunkArgs thunk) ++
                         " for a fn needing " ++ show a ++ ")" ++
                          showValue h thunk ++ "\n")
  
reduce' heapValue thunk@(Thunk { thunkExp = (CoreExp exp)}) = go exp
 where
  go (Var qVar)       = do let val = fromJustNote 
                                      ("Looking for var " ++ show qVar)
                                      (envLookup qVar $ thunkEnv thunk)
                           val' <- heapGet val
                           return $ reduceVar val' (thunkArgs thunk)
    where reduceVar val'@(Thunk {}) args = val' { thunkArgs = thunkArgs val'
                                                                      ++ args}
          reduceVar val' []              = val'
          reduceVar val' nonEmptyArgs    = error $ "losing args "
                                                     ++ (show $ thunkArgs thunk)
  go (Dcon qDcon)     = return $ DataValue { tag = qDcon
                                           , dataArgs = thunkArgs thunk }
  go (Lit lit)        = return $ reduceLit lit
  go (App exp arg)    = do newArg <- heapAdd (Thunk (thunkEnv thunk)
                                                    (CoreExp arg)
                                                    [])
                           return $ thunk { thunkArgs = newArg:(thunkArgs thunk)
                                          , thunkExp = CoreExp exp }

  go (Appt exp _)     = return $
                        thunk { thunkExp = CoreExp exp }
  go (Lam (Vb vbind) 
          exp)        = return $
                        case thunkArgs thunk of
                          arg:args -> let newEnv = envInsert (Nothing,
                                                                 fst vbind)
                                                             arg
                                                             (thunkEnv thunk)
                                       in Thunk newEnv (CoreExp exp) args
                          []       -> error $ "no arg for " ++ 
                                                 show (thunkExp thunk)
  go (Lam (Tb _) exp) = return $ thunk { thunkExp = CoreExp exp }
  go (Let vdefg exp)  = do rec { newEnv <- envFromVdefg newEnv
                                                        vdefg
                                                        (thunkEnv thunk) }
                           return $ thunk { thunkExp = CoreExp exp
                                          , thunkEnv = newEnv }
  go (Case exp vbind
             ty alts) = do scrutHeap <- heapAdd $ Thunk (thunkEnv thunk)
                                                        (CoreExp exp)
                                                        []
                           whnf scrutHeap
                           scrut <- heapGet scrutHeap
                           let envWithScrut = envInsert (Nothing, fst vbind)
                                                        scrutHeap
                                                        (thunkEnv thunk)
                           let alt = head $ [alt | alt <- alts
                                                 , matchingAlt alt scrut]
                           let newEnv = envWithAltBindings alt
                                                           scrut
                                                           envWithScrut
                           return $ thunk { thunkExp = CoreExp $ altExp alt
                                          , thunkEnv = newEnv }
  go (Cast exp _)     = return $ thunk { thunkExp = CoreExp exp }
  go (Note _ exp)     = return $ thunk { thunkExp = CoreExp exp }

reduceLit (Literal (Lint i) _) = IntegralValue i
reduceLit (Literal (Lchar c) _) = CharValue c
reduceLit (Literal (Lstring s) _) = StringValue s

matchingAlt :: Alt -> Value -> Bool
matchingAlt (Acon qDcon _ _ _)           (DataValue qDcon' _) = qDcon == qDcon'
matchingAlt (Alit (Literal (Lint i) _) _)  (IntegralValue i') = i == i'
matchingAlt (Alit (Literal (Lchar i) _) _) (CharValue i')     = i == i'
matchingAlt (Adefault _)       _                              = True

envWithAltBindings (Acon _ _ vbinds _) (DataValue _ args) env
  = foldr go env $ zip vbinds args
  where go (vbind, arg) env = envInsert (Nothing, fst vbind)
                                        arg
                                        env
envWithAltBindings _ _ env = env

altExp (Acon _ _ _ exp) = exp
altExp (Alit _ exp) = exp
altExp (Adefault exp) = exp

isThunkValue (Thunk {}) = True
isThunkValue _ = False


moduleMname (Module mname _ _) = mname

moduleById id mods = headNote ("Couldn't find module with id " ++ id) $
                       [ mod | mod <- mods, moduleId mod == id ]
moduleId (Module (M (_, _, id)) _ _) = id

coreFileContents filename = do
  file <- openFile filename ReadMode
  contents <- hGetContents file
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
  mods <- mapM coreFileContents ["B.hcr" ]
  --let result = eval mods "B" "initialzustate" []
  putStrLn "Starting"
  putStrLn $ showValueHeap $ runHeap (do
    arg <- eval mods "B" "initialzustate" []
    result <- eval mods "B" "appzustate" [arg]
    deepseq result
    heapGet result) 
  return ()
