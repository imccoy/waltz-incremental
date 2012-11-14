module Builtins where

import Language.Core.Core (Pname (P), AnMname (M))
import Types


mainPackage = P "main"
primPackage = P "ghczmprim"
basePackage = P "base"
integerPackage = P "integerzmgmp"
containersPackage = P "containerszm0zi4zi0zi0"
ghcTypesModule = M (primPackage, ["GHC"], "Types")
ghcBoolModule = M (primPackage, ["GHC"], "Bool")
ghcNumModule = M (basePackage, ["GHC"], "Num")
ghcBaseModule = M (basePackage, ["GHC"], "Base")
ghcListModule = M (basePackage, ["GHC"], "List")
ghcErrorModule = M (basePackage, ["GHC"], "Err")
ghcExceptionModule = M (basePackage, ["Control", "Exception"], "Base")
ghcMapModule = M (containersPackage, ["Data"], "Map")
ghcClassesModule = M (basePackage, ["GHC"], "Classes")
ghcGmpIntModule = M (integerPackage, ["GHC"], "Integer")
inctimeModule = M (mainPackage, [], "Inctime")
funcsModule = M (mainPackage, [], "Funcs")
funcsAuxModule = M (mainPackage, [], "FuncsAux")


boolTy = (Just ghcBoolModule, "Bool")
trueCon = (Just ghcBoolModule, "True")
falseCon = (Just ghcBoolModule, "False")

boolCon True = trueCon
boolCon False = falseCon

boolValue :: Bool -> Value
boolValue = (`DataValue` []) . boolCon

charPrimCon = (Just ghcTypesModule, "Czh")
intPrimCon = (Just ghcTypesModule, "Izh")

emptyList = (Just ghcTypesModule, "ZMZN")
consList = (Just ghcTypesModule, "ZC")


emptyListValue = DataValue emptyList []
consListValue h t = DataValue consList [h, t]



