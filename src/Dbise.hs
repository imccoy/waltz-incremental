{-# LANGUAGE ViewPatterns, TupleSections, ScopedTypeVariables #-}
module Dbise (dbiseCoreModule) where

import Data.List (isPrefixOf, isSuffixOf, isInfixOf)

import CoreSyn (Bind (Rec, NonRec), Expr (..))
import Name (occNameString, nameOccName, nameUnique, occNameSpace)
import Var
import Type
import TyCon (tyConName)

import Lookups
import Modify
import Names (mutantNameIntoSpace)
import TypeUtils
import Utils

dbiseCoreModule = modifyCoreModule $ ModifyFuncs {
  modifyCoreBinds = dbiseCoreBinds,
  modifyName = dbiseName,
  modifyLookupTyCon = return,
  modifyId = dbiseId,
  modifyDataCon = return,
  modifyTyCon = return . (,[],[]),
  modifyClass = id
}

dbiseName name = mutantNameIntoSpace name
                                     (occNameSpace $ nameOccName name)
                                     "dbised"

dbiseId id = do
  let name = dbiseName $ varName id
  let unique = nameUnique name
  type_ <- dbiseType $ varType id
  return $ case dbisableId id of
             True -> id `setVarType` type_
                        `setVarUnique` unique
                        `setVarName` name
             False -> id

dbiseType type_@(splitForAllTy_maybe -> Just _) = do
  let (tyVars, ty) = splitForAllTys type_
  type_' <- dbiseType ty
  return $ mkForAllTys tyVars $ type_'

dbiseType type_@(splitArrowAppTy_maybe -> Just (first, rest)) = do
  first' <- dbiseType first
  rest' <- dbiseType rest
  return $ mkArrow first' rest'

dbiseType type_@(dbisableType -> True) = do
  dbRefTC <- dbRefTyCon
  return $ mkTyConApp dbRefTC [type_]

dbiseType ty = return ty

dbisableType (splitTyConApp_maybe -> Just (tyCon, _))
  | "_incrementali" `isInfixOf` nameString = False
  | "#" `isSuffixOf` nameString            = False
  | otherwise                               = True
  where nameString = occNameString $ nameOccName $ tyConName tyCon

dbisableId id
  | "$f" `isPrefixOf` nameString      = False
  | "_dbise" `isInfixOf` nameString = False
  | otherwise                         = True
  where nameString = occNameString $ nameOccName $ varName id

dbiseCoreBinds = mapM dbiseCoreBind

dbiseCoreBind (NonRec name exp) = dbiseCoreBind $ Rec [(name, exp)]
dbiseCoreBind (Rec name_exps) = do
  undef <- lookupPreludeFn "GHC.Err" "undefined"
  newNameExps <- map2M dbiseId dbiseExp (filter (dbisableId . fst) name_exps) >>=
                   mapM (\(id, exp) -> return $ (id, (Var undef) `App` (Type $ varType id)))
  return $ Rec $ name_exps ++ newNameExps

dbiseExp exp = return exp
