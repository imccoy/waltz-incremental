{-# LANGUAGE Rank2Types, ViewPatterns #-}

module ToJsonString (toJsonString) where

import Data.Data
import Unsafe.Coerce (unsafeCoerce)
import Data.List (intersperse)

toJsonString :: (Data a) => a -> String
toJsonString a = toJsonString' (constrRep $ toConstr a)
  where toJsonString' (AlgConstr i) = "{" ++ tag i ++ " " ++ children a ++ "}"
        toJsonString' (IntConstr n) = "$hs.toHaskellInt(" ++ show n ++ ")"
        toJsonString' (FloatConstr n) = show n
        toJsonString' (CharConstr c) = ['\'', c, '\'']

tag i = "'tag': " ++ (show $ i)

needsTag (dataTypeRep . dataTypeOf -> (AlgRep cons)) = cons /= []
needsTag _ = False

children a = "'data': [" ++ 
             (concat $ intersperse "," $ children' a) ++
             "]"

children' a = gmapQ child a

child :: forall a. Data a => a -> String
child = toJsonString
