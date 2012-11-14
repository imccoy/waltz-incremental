{-# LANGUAGE ScopedTypeVariables, ViewPatterns, DoRec #-}
module Reduce where

import Db
import Types

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (foldrM)
import Safe

import Language.Core.Core

envFromVdefg :: Env -> Vdefg -> Env -> HeapM Env
envFromVdefg resultEnv vdefg env
  = case vdefg of
      (Rec vdefs)   -> foldrM envFromVdef env vdefs
      (Nonrec vdef) -> envFromVdef vdef env
  where envFromVdef (Vdef (qVar, ty, exp)) env
           = do v <- heapAdd $ Thunk resultEnv (CoreExp exp) []
                return $ envInsert qVar v env




whnf :: HeapValue -> HeapM ()
whnf heapValue = go =<< heapGet heapValue
  where go value | not (irreducibleValue value) = do reduce heapValue
                                                     whnf heapValue
                 | otherwise                    = return ()

deepseq :: HeapValue -> HeapM ()
deepseq heapValue = do go =<< (whnf heapValue >> heapGet heapValue)
  where go (DataValue con args)        = mapM_ deepseq args
        go v@(DatabaseValue _)         = reduce heapValue >> deepseq heapValue
        go v                           = return ()

irreducibleValue v@(Thunk _ (CoreExp (Lam (Vb _) _)) [])  = True
irreducibleValue v@(Thunk _ (CoreExp _) _)                = False
irreducibleValue v@(Thunk _ rt@(RtExp _ _ _ _) args)
          | rtExpArity rt > length args              = True
          | otherwise                                = False
irreducibleValue (DatabaseValue _) = False
irreducibleValue _ = True

reduce :: HeapValue -> HeapM ()
reduce heapValue = heapChange (reduce' heapValue) heapValue
{-
reduce heapValue = do v <- heapGet heapValue
                      h <- heapGetFull
                      liftIO $ putStrLn $ "before " ++ show heapValue
                                                ++ " " ++ showValue h v
                      v' <- reduce' heapValue v
                      h' <- heapGetFull
                      liftIO $ v' `seq` putStrLn $ "after " ++ show heapValue
                                                ++ " " ++ showValue h' v'
                      heapSet heapValue v'
-}

reduce' heapValue v@(DatabaseValue id) = do
  constructor <- liftIO $ retrieveValue id
  constructor
                                            
reduce' heapValue v@(isThunkValue -> False)                 = return v

reduce' heapValue thunk@(Thunk { thunkExp = (RtExp f _ _ a) })= do
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
                           liftIO $ putStrLn $ "resolved var to " ++ show val
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
                           let alt = headNote ("no matching alt" ++ show vbind)
                                              [alt | alt <- alts
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
matchingAlt (Alit (Literal lit _) _) v = error $ "type error: literal " ++ 
                                                     show lit ++ ", value " ++
                                                     showValueShort v
matchingAlt (Acon qDcon _ _ _) v = error $ "type error: dcon " ++ 
                                                     show qDcon ++ ", value " ++
                                                     showValueShort v

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


