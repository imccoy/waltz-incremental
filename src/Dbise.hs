{-# LANGUAGE ViewPatterns, TupleSections, ScopedTypeVariables #-}
module Dbise (dbiseCoreModule) where

import Control.Monad (liftM, liftM2)
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)

import CoreSyn (Bind (Rec, NonRec), Expr (..))
import Name (occNameString, nameOccName, nameUnique, occNameSpace)
import Var
import Type
import TyCon (tyConName)

import Debug.Trace
import Outputable

import Lookups
import Modify
import Names (mutantNameIntoSpace)
import ExprUtils
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

dbiseType type_@(dbisableType -> False) = return type_

dbiseType type_@(splitForAllTys -> (tyVars@(_:_), ty)) = do
  ty' <- dbiseType ty
  return $ mkForAllTys tyVars $ ty'

dbiseType type_@(splitArrowAppTy_maybe -> Just (first, rest)) = do
  first' <- dbiseType first
  rest' <- dbiseType rest
  return $ mkArrow first' rest'

dbiseType type_@(splitAppTy_maybe -> Just (first, rest)) = do
  first' <- return first
  rest' <- return rest
  return $ mkAppTy first' rest'


dbiseType type_@(splitTyConApp_maybe -> Just (tyCon, args)) = do
  dbRefTC <- dbRefTyCon
  return $ mkTyConApp dbRefTC [type_]

dbiseType type_ = return type_

dbisableType (splitForAllTys -> ((_:_), ty)) = dbisableType ty

dbisableType (splitAppTy_maybe -> Just (ty, _)) = dbisableType ty

dbisableType (splitTyConApp_maybe -> Just (tyCon, _))
  | "_incrementali" `isInfixOf` nameString  = trace ("failing1 " ++ nameString) False
  | "#" `isSuffixOf` nameString             = trace ("failing2 " ++ nameString) False
  | "T:" `isPrefixOf` nameString            = trace ("failing3 " ++ nameString) False
  | "Incrementalised" `isPrefixOf` nameString = trace ("failing4 " ++ nameString) False
  | otherwise                               = trace ("passing name " ++ nameString) True
  where nameString = occNameString $ nameOccName $ tyConName tyCon
dbisableType t = trace ("passing " ++ (showSDoc $ ppr t)) True

dbisableId id
  | not $ dbisableType (varType id)           = False
  | "$f" `isPrefixOf` nameString              = False
  | "_dbise" `isInfixOf` nameString           = False
  | "Incrementalised" `isPrefixOf` nameString = False
  -- so ghetto:
  | "extractReplaceValue" == nameString       = False
  | "isIncrementalisedReplace" == nameString  = False
  | "isIncrementalisedIdentity" == nameString = False
  | "mkIncrementalisedReplace" == nameString  = False
  | "mkIncrementalisedIdentity" == nameString = False
  | "allIdentityOrReplace" == nameString = False
  | otherwise                                 = True
  where nameString = occNameString $ nameOccName $ varName id

dbiseCoreBinds = mapM dbiseCoreBind

dbiseCoreBind (NonRec name exp) = dbiseCoreBind $ Rec [(name, exp)]
dbiseCoreBind (Rec name_exps) = do
  newNameExps <- map2M dbiseId dbiseExp (filter (dbisableId . fst) name_exps)
  return $ Rec $ name_exps ++ newNameExps

dbiseExp (Var id) = liftM Var (dbiseId id)
dbiseExp (Lit lit) = return $ Lit lit
dbiseExp app@(App exp arg)
  | ((Var v), _, _) <- collectTypeAndValArgs app,
    not (dbisableId v)     = return app
  | otherwise              = liftM2 App (dbiseExp exp) (dbiseExp arg)
dbiseExp (Lam arg body) = liftM2 Lam (dbiseId arg) (dbiseExp body)
dbiseExp (Let bind exp) = liftM2 Let (dbiseCoreBind bind) (dbiseExp exp)
dbiseExp (Case scrut bind type_ alts) = do
  undef <- lookupPreludeFn "GHC.Err" "undefined"
  ty' <- dbiseType type_
  return $ (Var undef) `App` (Type $ ty')
dbiseExp (Cast expr co) = liftM2 Cast (dbiseExp expr) (return co)
dbiseExp (Note note expr) = dbiseExp expr
dbiseExp (Type type_) = liftM Type (dbiseType type_)
