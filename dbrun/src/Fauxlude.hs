module Fauxlude where

import Prelude hiding (map)
import Reduce
import Funcs ()

import Builtins
import Types

import Control.Monad (liftM2)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (foldrM)
import Safe

import Language.Core.Core (AnMname, Id, Exp (..))

builtinsEnv :: Env -> HeapM Env
builtinsEnv hsEnv
   = addBuiltin ghcBaseModule "unpackCStringzh" unpackCString 1 envEmpty >>=
     addBuiltin ghcBaseModule "ord" ord 1 >>=
     addBuiltin ghcErrorModule "error" patError 1 >>=
     addBuiltin ghcExceptionModule "patError" patError 1 >>=
     addBuiltinH ghcNumModule "zp" (tcThunk 0) 1 >>=
     addBuiltinH ghcNumModule "fromInteger" (tcThunk 6) 1 >>=
     addBuiltinM ghcNumModule "zdfNumInt" zdfNumInteger 0 >>=
     addBuiltinFn ghcListModule "length" "listLength" hsEnv >>=
     addBuiltinFn ghcListModule "filter" "listFilter" hsEnv >>=
     addBuiltinFn ghcListModule "all" "listAll" hsEnv >>=
     addBuiltinFn ghcBaseModule "map" "listMap" hsEnv >>=
     addBuiltinF ghcBaseModule "foldr" hsEnv >>=
     addBuiltinFn ghcClassesModule "zdfOrdChar" "zdfOrdrChar" hsEnv >>=
     addBuiltinFn ghcClassesModule "zdfOrdZMZN" "zdfOrdrZMZN" hsEnv >>=
     addBuiltinFn ghcClassesModule "zdfEqZMZN" "zdfEqlZMZN" hsEnv >>=
     addBuiltinFn ghcClassesModule "zdfEqChar" "zdfEqlChar" hsEnv >>=
     addBuiltinF ghcClassesModule "zeze" hsEnv >>=
     addBuiltinF ghcBaseModule "zd" hsEnv >>=
     addBuiltinFn ghcMapModule "insert" "mapInsert" hsEnv >>=
     addBuiltinF ghcMapModule "mapWithKey" hsEnv >>=
     addBuiltinFn ghcMapModule "alter" "mapAlter" hsEnv >>=
     addBuiltinFn ghcMapModule "empty" "mapEmpty" hsEnv >>=
     addBuiltinFn ghcMapModule "lookup" "mapGet" hsEnv >>=
     addBuiltin funcsAuxModule "intEq" intEq 2 >>=
     addBuiltin funcsAuxModule "intPlus" intPlus 2 >>=
     addBuiltin ghcGmpIntModule "smallInteger" smallInteger 1

addBuiltinM mod name value arity env = do
  v <- value
  addBuiltin mod name v arity env

addBuiltin :: AnMname -> Id -> ([Value] -> HeapM Value) -> Int -> Env
           -> HeapM Env
addBuiltin mod name f arity env = addBuiltinH mod
                                              name
                                              (\a -> do heapGetArgs f a)
                                              arity
                                              env

heapGetArgs f a = do mapM whnf a
                     liftIO $ putStrLn $ "heapGetArgs forcing " ++ show a
                     f =<< mapM heapGet a

addBuiltinH :: AnMname -> Id -> ([HeapValue] -> HeapM Value) -> Int -> Env
            -> HeapM Env
addBuiltinH mod name f arity env = do
  heapValue <- heapAdd (Thunk envEmpty (RtExp f mod name arity) [])
  return $ envInsert (Just mod, name) heapValue env

addBuiltinF mod name = addBuiltinFn mod name name

addBuiltinFn :: AnMname -> Id -> Id -> Env -> Env -> HeapM Env
addBuiltinFn mod name implName hsEnv env = do
  heapValue <- heapAdd (Thunk hsEnv (CoreExp $ Var (Just funcsModule, implName)) [])
  return $ envInsert (Just mod, name) heapValue env


unpackCString [StringValue s] = foldrM go emptyListValue s
  where go c s = liftM2 consListValue (do c' <- heapAdd $ CharValue c
                                          heapAdd $ DataValue charPrimCon [c'])
                                      (heapAdd s)
unpackCString args = badArgs "unpackCStringzh" args

ord [CharValue c] = return $ IntegralValue $ fromIntegral $ fromEnum c
ord [DataValue _ [c]] = do whnf c
                           c' <- heapGet c
                           ord [c']

patError [s@(DataValue _ _)] = error =<< heapToNativeString s
patError [StringValue s] = error s
patError args = badArgs "patError" args

heapToNativeString (DataValue con args)
 | con == emptyList = return ""
 | con == consList = do mapM_ whnf [h, t]
                        h' <- heapToNativeChar =<< heapGet h
                        t' <- heapToNativeString =<< heapGet t
                        return (h':t')
 where [h, t] = args

heapToNativeChar (DataValue _ [arg]) = heapToNativeChar =<< heapGet arg
heapToNativeChar (CharValue c) = return c
                       

smallInteger [IntegralValue i] = return $ IntegralValue i
smallInteger args = badArgs "smallInteger" args

intEq [IntegralValue i, IntegralValue j] = return $ boolValue $ i == j
intEq args = badArgs "intEq" args

intPlus [IntegralValue a, IntegralValue b] = return $ IntegralValue $ a + b
intPlus [DataValue _ [a], DataValue _ [b]] = do whnf a
                                                whnf b
                                                a' <- heapGet a
                                                b' <- heapGet b
                                                r' <- heapAdd =<< intPlus [a', b']
                                                return $ DataValue intPrimCon [r']
intPlus args = badArgs "intPlus" args

charEq [CharValue i, CharValue j] = return $ boolValue $ i == j
charEq args = badArgs "charEq" args

zdfNumInteger = do
  addrs <- mapM (\(f,a,n) -> heapAdd (Thunk envEmpty
                                      (RtExp (heapGetArgs f) undefined n a)
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
    zpInteger args@[IntegralValue a, IntegralValue b] = intPlus args
    zpInteger args@[DataValue _ [a], DataValue _ [b]] = do mapM whnf [a, b]
                                                           intPlus =<< mapM heapGet 
                                                                            [a,b]
    zpInteger args = badArgs "zpInteger" args
    fromIntegerInteger [IntegralValue a] = do a' <- heapAdd $ IntegralValue a
                                              return $ DataValue intPrimCon [a']
    fromIntegerInteger args = badArgs "fromIntegerInteger" args



tcDictMethod :: Int -> Value -> HeapM HeapValue
tcDictMethod n (DataValue v methods) = return $
                                       atNote ("Can't get method " ++ show n ++
                                                   " of " ++ show v)
                                              methods
                                              n
tcDictMethod _ v = badArgs "tcDictMethod" [v]

tcThunk :: Int -> [HeapValue] -> HeapM Value
tcThunk n a@(dict:args) = do whnf dict
                             addThunkArgs args =<<
                                  (heapGet dict
                                        >>= tcDictMethod n
                                        >>= heapGet)

