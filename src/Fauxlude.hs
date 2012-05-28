module Fauxlude where

import Builtins
import Types

import Safe


baseFunc ["GHC"] "Base" "unpackCStringzh" = unpackCString
baseFunc ["GHC"] "Num" "zp" = zp
baseFunc ["GHC"] "Num" "zdfNumInteger" = zdfNumInteger
baseFunc ["GHC"] "Num" "fromInteger" = fromInteger'
baseFunc modsNames modName fName
  = error $ "Couldn't find base func " ++ show modsNames ++ " " ++
                    modName ++ " " ++ fName

integerFunc ["GHC"] "Integer" "smallInteger" = smallInteger

unpackCString [StringValue s] = foldr go emptyListValue s
  where go c s = consListValue (CharValue c) s
unpackCString xs = error $ "unpackCString not defined for " ++ show xs

smallInteger [IntegralValue i] = IntegralValue i
smallInteger xs = error $ "smallInteger not defined for " ++ show xs

zp [dict, a, b] = addThunkArgs (tcDictMethod dict 0) [a, b]
fromInteger' [dict, a] = addThunkArgs (tcDictMethod dict 6) [a]

zdfNumInteger _ = DataValue (Just ghcNumModule, "zdcNum")
                            [zpInteger
                            ,undefined -- subtract
                            ,undefined -- multiply
                            ,undefined -- negate
                            ,undefined -- abs
                            ,undefined -- signum
                            ,fromIntegerInteger]
zpInteger = Thunk envEmpty (RtExp go) []
  where go [IntegralValue a, IntegralValue b] = IntegralValue $ a + b
        go args = error $ "zpInt not defined for " ++ show args
fromIntegerInteger = Thunk envEmpty (RtExp go) []
  where go [IntegralValue a] = IntegralValue $ a
        go args = error $ "zpInt not defined for " ++ show args

tcDictMethod (DataValue v methods) n = atNote ("Can't get method " ++ show n ++
                                                   " of " ++ show v)
                                              methods
                                              n
tcDictMethod v _ = error $ "Can't get tcDictMethod of " ++ show v

addThunkArgs t [] = t
addThunkArgs t@(Thunk {}) a = t { thunkArgs = thunkArgs t ++ a }
addThunkArgs v a = error $  "Can't add to non-thunk " ++ show v ++
                                " args " ++ show a


