module Fauxlude where

import Builtins
import Types

import Safe


baseFunc ["GHC"] "Base" "unpackCStringzh" [StringValue s]
  = foldr go emptyListValue s
  where go c s = consListValue (CharValue c) s
baseFunc ["GHC"] "Num" "zp"               (dict:args)
  = tcThunk dict 0 args
baseFunc ["GHC"] "Num" "fromInteger"      (dict:args)
  = tcThunk dict 6 args
baseFunc ["GHC"] "Num" "zdfNumInteger" _
  = DataValue (Just ghcNumModule, "zdcNum") $
              map (\f -> Thunk envEmpty (RtExp f) [])
                  [zpInteger
                  ,undefined -- subtract
                  ,undefined -- multiply
                  ,undefined -- negate
                  ,undefined -- abs
                  ,undefined -- signum
                  ,fromIntegerInteger]

baseFunc ["GHC"] "Integer" "smallInteger" [IntegralValue i]
  = IntegralValue i

baseFunc modsNames modName fName args
  = error $ "Couldn't find base func " ++ show modsNames ++ " " ++
                    modName ++ " " ++ fName ++ " " ++ show args


zpInteger [IntegralValue a, IntegralValue b] = IntegralValue $ a + b
zpInteger args = error $ "zpInt not defined for " ++ show args
fromIntegerInteger [IntegralValue a] = IntegralValue a
fromIntegerInteger args = error $ "zpInt not defined for " ++ show args

tcDictMethod (DataValue v methods) n = atNote ("Can't get method " ++ show n ++
                                                   " of " ++ show v)
                                              methods
                                              n
tcDictMethod v _ = error $ "Can't get tcDictMethod of " ++ show v

addThunkArgs t [] = t
addThunkArgs t@(Thunk {}) a = t { thunkArgs = thunkArgs t ++ a }
addThunkArgs v a = error $  "Can't add to non-thunk " ++ show v ++
                                " args " ++ show a
tcThunk dict n args = addThunkArgs (tcDictMethod dict n) args

