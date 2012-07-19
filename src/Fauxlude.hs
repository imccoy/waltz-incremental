module Fauxlude where

import Builtins
import Types

import Control.Monad (liftM)
import Safe

import Language.Core.Core (AnMname, Id)

builtinsEnv :: HeapM Env
builtinsEnv
   = addBuiltin ghcBaseModule "unpackCStringzh" unpackCString 1 envEmpty >>=
     addBuiltinH ghcBaseModule "zd" zd 2 >>=
     addBuiltinH ghcClassesModule "zaza" zaza 2 >>=
     addBuiltinH ghcClassesModule "zeze" (tcThunk 0) 1 >>=
     addBuiltinM ghcClassesModule "zdfEqChar" zdfEqChar 0 >>=
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

unpackCString [StringValue s] =  mkRuntimeString s
unpackCString args = badArgs "unpackCStringzh" args

zd :: [HeapValue] -> HeapM Value
zd [a, b] = do a' <- heapGet a
               addThunkArgs [b] a'

zaza :: [HeapValue] -> HeapM Value
zaza [a, b] = do a' <- heapGet a
                 case truthy a' of
                   True -> heapGet b
                   False -> return $ untruthy False

smallInteger [IntegralValue i] = return $ IntegralValue i
smallInteger args = badArgs "smallInteger" args

badArgs :: String -> [Value] -> HeapM a
badArgs n [] = error $ n ++ " not defined for []"
badArgs n args = heapGetFull >>= (\h -> error $ n ++ " not defined for "
                                            ++ concat (map (showValue h) args))
zdfMk (f,a,n) = heapAdd (Thunk envEmpty
                               (RtExp (heapGetArgs f) n a)
                               [])

zdfEqChar = do
  addrs <- mapM zdfMk
                [(zezeChar,2,"zezeChar")]
  return $ (\_ -> return $ DataValue (Just ghcClassesModule, "DZCEq") addrs)
  where zezeChar [CharValue a, CharValue b] = return $ untruthy $ a == b 

zdfNumInteger = do
  addrs <- mapM zdfMk
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




tcDictMethod :: Int -> Value -> HeapM HeapValue
tcDictMethod n (DataValue v methods) = return $
                                       atNote ("Can't get method " ++ show n ++
                                                   " of " ++ show v)
                                              methods
                                              n
tcDictMethod _ v = badArgs "tcDictMethod" [v]

addThunkArgs :: [HeapValue] -> Value -> HeapM Value
addThunkArgs [] t = return t
addThunkArgs a t@(Thunk {thunkArgs = a'}) = return $ t { thunkArgs = a' ++ a }
addThunkArgs a v = badArgs "addThunkArgs" =<< liftM (v:) (mapM heapGet a)

tcThunk :: Int -> [HeapValue] -> HeapM Value
tcThunk n a@(dict:args) = addThunkArgs args =<<
                                (heapGet dict
                                      >>= tcDictMethod n
                                      >>= heapGet)

