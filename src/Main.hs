{-# LANGUAGE ScopedTypeVariables, ViewPatterns, DoRec #-}
module Main where

import Builtins
import Fauxlude
import Types

import Control.Monad (when, forM_)
import qualified Data.Map as Map
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
             forM_ a1 $ (\v -> (heapValue, thunk) `dependsOn` (v, "reduce' LT"))
             addThunkArgs a2 =<< f a1
    EQ -> do v <- f $ thunkArgs thunk
             forM_ (thunkArgs thunk) (\v -> (heapValue, thunk) `dependsOn`
                                                          (v, "reduce' EQ"))
             return v
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
                           (heapValue, thunk) `dependsOn` (val, "Var")
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

                           -- (heapValue, thunk) `dependsOn` (newArg, "App")
                           return $ thunk { thunkArgs = newArg:(thunkArgs thunk)
                                          , thunkExp = CoreExp exp }
  go (Appt exp _)     = return $ thunk { thunkExp = CoreExp exp }
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
                           (heapValue, thunk) `dependsOn` (scrutHeap, "Case scrut")
                           whnf scrutHeap
                           scrut <- heapGet scrutHeap
                           let envWithScrut = envInsert (Nothing, fst vbind)
                                                        scrutHeap
                                                        (thunkEnv thunk)
                           let matchingAlts = [alt | alt <- alts
                                              , matchingAlt alt scrut]
                           let alt = case matchingAlts of
                                       [] -> error $ "No alt matching " ++ show exp ++ 
                                                       " (tried " ++
                                                       valueConstr scrut ++
                                                       " against " ++ 
                                                       show alts ++ ")"
                                       (a:_) -> a
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
-- is ditching the namespace safe in this first one? Sure is. 
-- All datacons from a type come from the same module, and the
-- type system guarantees that the scrutinee is of the same type
-- as the Acon's data constructor
matchingAlt (Acon qDcon _ _ _)
            (DataValue qDcon' _)           = snd qDcon == snd qDcon'
matchingAlt (Alit (Literal (Lint i) _) _)  
            (IntegralValue i')             = i == i'
matchingAlt (Alit (Literal (Lchar i) _) _)
            (CharValue i')                 = i == i'
matchingAlt (Adefault _)       
            _                              = True

envWithAltBindings (Acon _ _ vbinds _) (DataValue _ args) env
  = foldr go env $ zip vbinds args
  where go (vbind, arg) env = envInsert (Nothing, fst vbind)
                                        arg
                                        env
envWithAltBindings _ _ env = env

change :: HeapValue -> Value -> HeapM ()
change heapValue newValue = do
  oldValue <- heapGet heapValue
  when (oldValue /= newValue) $ do
    thunkDeps <- heapGetDeps heapValue
    heapSet heapValue newValue
    updateDependents thunkDeps
  where updateDependents = updateDependents' . Map.minViewWithKey
        updateDependents' Nothing = return ()
        updateDependents' (Just ((thunk, addrsThunks), thunkDeps)) = do
          forM_ addrsThunks $ \(addr, thunk) ->
            change addr thunk
          updateDependents thunkDeps


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

findListEmpty heapValue = do
  value <- heapGet heapValue
  go value
  where go (DataValue tag args) | tag == emptyList = return heapValue
                                | tag == consList  = findListEmpty $ last args
                                | otherwise        = error $ "findListEmpty " ++
                                                              "on non-list"
        go v = error $ "findListEmpty on non-datavalue " ++ (valueConstr v)

initialValue mods = do
  arg <- eval mods "B" "initialzustate" []
  deepseq arg
  result <- eval mods "B" "appzustate" [arg]
  deepseq result
  return (arg, result)

addValue mods arg result = do
  newInputWord <- heapAdd =<< mkRuntimeString "Dog"
  newInputDefinition <- heapAdd =<< mkRuntimeString "Wolf"
  newInput <- heapAdd (DataValue (Nothing, "NewDefinition")
                                 [newInputWord, newInputDefinition])
  newBottom <- heapAdd emptyListValue
  argBottom <- findListEmpty arg
  initialArgValue <- heapGet arg
  let newArgValue = (consListValue newInput newBottom)
  change argBottom newArgValue
  deepseq result
  return (newArgValue, result)

main = do
  mods <- mapM coreFileContents ["B.hcr" ]
  putStrLn "Starting"
  let ((input, value), heap) = runHeapEmpty $ initialValue mods
  putStrLn $ "Starting output =\t\t\t" ++
                showValue heap (fst $ runHeap heap $ heapGet value)
  let ((input', value'), heap') = runHeap heap $ addValue mods input value
  putStrLn $ "Output after prepending to list =\t" ++
    showValue heap' (fst $ runHeap heap' $ heapGet value')
  return ()
