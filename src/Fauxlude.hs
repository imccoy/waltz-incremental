module Fauxlude where

import Builtins
import Types

import Control.Monad (liftM, liftM2)
import Data.Foldable (foldrM)
import Safe

import Language.Core.Core (AnMname, Id)

builtinsEnv :: HeapM Env
builtinsEnv
   = addBuiltin ghcBaseModule "unpackCStringzh" unpackCString 1 envEmpty >>=
     addBuiltinH ghcNumModule "zp" (tcThunk 0) 1 >>=
     addBuiltinH ghcNumModule "fromInteger" (tcThunk 6) 1 >>=
     addBuiltinM ghcNumModule "zdfNumInteger" zdfNumInteger 0 >>=
     addBuiltin ghcGmpIntModule "smallInteger" smallInteger 1


addBuiltinM mod name value arity env = do
  v <- value
  addBuiltin mod name v arity env

addBuiltin :: AnMname -> Id -> ([Value] -> HeapM Value) -> Int -> Env
           -> HeapM Env
addBuiltin mod name f arity env = addBuiltinH mod name (heapGetArgs f) arity env

heapGetArgs f a = f =<< mapM heapGet a

addBuiltinH :: AnMname -> Id -> ([HeapValue] -> HeapM Value) -> Int -> Env
            -> HeapM Env
addBuiltinH mod name f arity env = do
  heapValue <- heapAdd (Thunk envEmpty (RtExp f name arity) [])
  return $ envInsert (Just mod, name) heapValue env

unpackCString [StringValue s] = foldrM go emptyListValue s
  where go c s = liftM2 consListValue (heapAdd $ CharValue c) (heapAdd s)
unpackCString args = badArgs "unpackCStringzh" args

smallInteger [IntegralValue i] = return $ IntegralValue i
smallInteger args = badArgs "smallInteger" args

badArgs :: String -> [Value] -> HeapM a
badArgs n [] = error $ n ++ " not defined for []"
badArgs n args = heapGetFull >>= (\h -> error $ n ++ " not defined for "
                                            ++ concat (map (showValue h) args))

zdfNumInteger = do
  addrs <- mapM (\(f,a,n) -> heapAdd (Thunk envEmpty
                                      (RtExp (heapGetArgs f) n a)
                                      []))
                [(zpInteger,2,"zpInteger")
                ,(undefined,undefined,"subtract") -- subtract
                ,(undefined,undefined,"multiply") -- multiply
                ,(undefined,undefined,"negate") -- negate
                ,(undefined,undefined,"abs") -- abs
                ,(undefined,undefined,"signum") -- signum
                ,(fromIntegerInteger,1,"fromIntegerInteger")]

  return $ (\_ -> return $ DataValue (Just ghcNumModule, "DZCNum") addrs)
  where
    zpInteger [IntegralValue a, IntegralValue b] = return $ IntegralValue
                                                                $ a + b
    zpInteger args = badArgs "zpInteger" args
    fromIntegerInteger [IntegralValue a] = return $ IntegralValue a
    fromIntegerInteger args = badArgs "fromIntegerInteger" args




tcDictMethod :: Value -> Int -> HeapM HeapValue
tcDictMethod (DataValue v methods) n = return $
                                       atNote ("Can't get method " ++ show n ++
                                                   " of " ++ show v)
                                              methods
                                              n
tcDictMethod v _ = badArgs "tcDictMethod" [v]

addThunkArgs :: [HeapValue] -> HeapValue -> HeapM Value
addThunkArgs [] t = heapGet t
addThunkArgs a v = heapGet v >>= go
  where go t@(Thunk {thunkArgs = a'}) = return $ t { thunkArgs = a' ++ a }
        go v = badArgs "addThunkArgs" =<< liftM (v:) (mapM heapGet a)

tcThunk :: Int -> [HeapValue] -> HeapM Value
tcThunk n a@(dict:args) = addThunkArgs args =<<
                                ((\d -> tcDictMethod d n) =<< heapGet dict)

