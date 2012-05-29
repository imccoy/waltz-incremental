module Builtins where

import Language.Core.Core (Pname (P), AnMname (M))
import Types

primPackage = P "ghczmprim"
basePackage = P "base"
integerPackage = P "integerzmgmp"
ghcTypesModule = M (primPackage, ["GHC"], "Types")
ghcNumModule = M (basePackage, ["GHC"], "Num")
ghcBaseModule = M (basePackage, ["GHC"], "Base")
ghcGmpIntModule = M (integerPackage, ["GHC"], "Integer")

emptyList = (Just ghcTypesModule, "ZMZN")
consList = (Just ghcTypesModule, "ZC")

emptyListValue = DataValue emptyList []
consListValue h t = DataValue consList [h, t]

