module Builtins where

import Control.Monad (liftM2)
import Data.Foldable (foldrM)

import Language.Core.Core (Pname (P), AnMname (M))
import Types

primPackage = P "ghczmprim"
basePackage = P "base"
integerPackage = P "integerzmgmp"
ghcTypesModule = M (primPackage, ["GHC"], "Types")
ghcNumModule = M (basePackage, ["GHC"], "Num")
ghcBaseModule = M (basePackage, ["GHC"], "Base")
ghcBoolModule = M (primPackage, ["GHC"], "Bool")
ghcClassesModule = M (basePackage, ["GHC"], "Classes")
ghcGmpIntModule = M (integerPackage, ["GHC"], "Integer")

emptyList = (Just ghcTypesModule, "ZMZN")
consList = (Just ghcTypesModule, "ZC")

emptyListValue = DataValue emptyList []
consListValue h t = DataValue consList [h, t]

boolDatacon = (Just ghcBoolModule, "Bool")
trueBool = (Just ghcBoolModule, "True")
falseBool = (Just ghcBoolModule, "False")

truthy (DataValue a _) = a == trueBool

untruthy True = DataValue trueBool []
untruthy False = DataValue falseBool []


mkRuntimeString :: String -> HeapM Value
mkRuntimeString = foldrM go emptyListValue
  where go c s = liftM2 consListValue (heapAdd $ CharValue c) (heapAdd s)
