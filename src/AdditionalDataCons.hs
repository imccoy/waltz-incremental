{-# LANGUAGE ViewPatterns #-}
module AdditionalDataCons where

import qualified Data.List as List

import CoreSyn
import DataCon
import Name hiding (varName)
import Outputable
import TyCon
import Type

data AdditionalConType = AddConReplacement
                       | AddConHoist
                       | AddConIdentity
  deriving Show
additionalConTypes = [AddConReplacement, AddConHoist, AddConIdentity]
additionalConSuffix AddConReplacement = "replace"
additionalConSuffix AddConHoist       = "hoist"
additionalConSuffix AddConIdentity    = "identity"


builderMutantDataConIndexes dataCon = map fst index_types
  where all_index_types = zip [0..] (dataConOrigArgTys dataCon)
        index_types = filter (\(i, t) -> tyTyConMatches t tyCon)
                             all_index_types
        tyCon = dataConTyCon dataCon


tyTyConMatches (splitTyConApp_maybe -> Just (tyCon1, _)) tyCon2
  = tyCon1 == tyCon2
tyTyConMatches _ _ = False



lookupDataCon type_ matches err
  = let tyCon = case (splitTyConApp_maybe type_) of
                  Just (con, _) -> con
                  otherwise     -> error $ "so confused " ++ 
                                           (showSDoc $ ppr $ type_)
        cons | isAlgTyCon tyCon = data_cons $ algTyConRhs tyCon
             | otherwise = error $ "not an alg ty con " ++
                                   (showSDocDebug $ ppr tyCon) ++
                                   " when looking for " ++ err
        nameString c = occNameString $ nameOccName $ dataConName c
        matchingCon c = matches (nameString c)
     in case filter matchingCon cons of
          (con:_) -> Just con
          []      -> Nothing 
 



lookupDataConBySuffix type_ suffix
  = lookupDataCon type_ (List.isSuffixOf suffix) suffix
 

lookupDataConByAdd type_ additionalCon
 = case lookupDataConBySuffix type_ (additionalConSuffix additionalCon) of
     Just a    -> a
     otherwise -> error $ "Couldn't find " ++ show additionalCon ++
                          " for " ++ (showSDoc $ ppr $ type_)
 
lookupDataConByBuilderIndex type_ builderIndex
 = case lookupDataConBySuffix type_ (builderConSuffix builderIndex) of
     Just a    -> a
     otherwise -> error $ "Couldn't find builder " ++ show builderIndex ++
                          " for " ++ (showSDoc $ ppr $ type_)

dataConAtType con type_ = foldl (\e t -> App e (Type t))
                                (Var $ dataConWrapId con)
                                typeArgs
  where typeArgs = case splitTyConApp_maybe type_ of
                     Just (_, args) -> args
                     otherwise      -> []


builderConSuffix n = "build_using_" ++ show n
