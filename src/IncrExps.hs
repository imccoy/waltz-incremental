{-# LANGUAGE ViewPatterns, TupleSections, ScopedTypeVariables, DoRec #-}

module IncrExps where

import Control.Monad
import qualified Data.List as List
import Data.Maybe
import Safe


import GHC (getName)
import CoreFVs (exprFreeIds)
import CoreSyn
import DataCon
import HscTypes (tyThingId)
import Id
import IdInfo (IdDetails (DataConWrapId, DataConWorkId))
import MkCore (mkListExpr)
import MkId (realWorldPrimId)
import Outputable
import PrelNames (statePrimTyConKey)
import Type
import Unique (getUnique)
import Var
import VarSet (elemVarSet)



import AdditionalDataCons
import Lookups
import IncrTypes
import TypeUtils
import Tracer
import Utils


mutantCoreBinds = mapM mutantCoreBind

mutantCoreBind corebind@(NonRec name exp) = do
  name' <- mutantCoreBndr name
  exp' <- mutantExp exp
  return $ Rec [(name,exp), (name', exp')]
mutantCoreBind corebinds@(Rec name_exps) = do
  newNameExps <- map2M mutantCoreBndr mutantExp name_exps
  return $ Rec $ name_exps ++ newNameExps

mutantCoreBndr = mutantId

mutantExp :: Expr CoreBndr -> TypeLookupM (Expr CoreBndr)

mutantExp (Var id)
  | (DataConWrapId dc) <- idDetails id = dataCon dc
  | (DataConWorkId dc) <- idDetails id = dataCon dc
  | otherwise                          = liftM Var $ lookupOrMutantId id
  where dataCon dc = dataConMkFuncId dc >>=
                       return . Var .  tyThingId . fromJustNote ("mutantExp Var "
                                                                  ++ show id)


mutantExp app@(App expr arg)
  = monadGuard [
      (return $ isIncBoxApp app ,mutantIncBoxApp app)
     ,(return $ isPrimCon expr  ,mutantPrimCon expr arg)
     ,(return $ isTypeArg arg   ,mutantTypeApp $ collectTypeArgs app)
     ,(return True, do
         expr' <- mutantExp expr
         arg' <- mutantExp arg
         return $ expr' `App` arg `App` arg')]
  where
    mutantTypeApp (exp, args) = do (tyVars, dicts) <- foldM (mutantTypeApp' exp)
                                                            ([], [])
                                                            args
                                   exp' <- mutantExp exp
                                   return $ mkApps exp' (reverse tyVars ++ reverse dicts)
    mutantTypeApp' exp (tyVars, dicts) arg = do
      arg' <- mutantExp arg
      let tyVars' = if isRealWorld $ exprType arg
                         then arg:tyVars
                         else arg':arg:tyVars
      dict <- expIncrementalisedDictionary arg
      let dicts' = if isDataConExp exp || isRealWorld (exprType arg)
                     then dicts
                     else dict:dicts
      return (tyVars', dicts')

-- for \ a -> b, need to check if all args are identity
-- If so, produce an identity of the result type.
-- We do a bit of an eta-expand-ish thing here (with additionalVarTys)
mutantExp expr@(Lam _ _) = mutantLam expr
mutantExp (Let bind expr) = liftM2 Let
                                   (mutantCoreBind bind)
                                   (mutantExp expr)
mutantExp c@(Case expr id type_ alts) = liftM4 Case 
                                                (mutantExp expr)
                                                (mutantCoreBndr id)
                                                (mutantType type_)
                                                (introduceSpecialAltCases c =<<
                                                 mutantAlts (expr, id) alts)
mutantExp (Cast expr coercion) = liftM2 Cast
                                         (mutantExp expr)
                                         (mutantType coercion)
mutantExp (Type type_) = liftM Type (mutantType type_)
mutantExp (Note note expr) = liftM (Note note) (mutantExp expr)
mutantExp (Lit lit) = return (Lit lit)

mutantLam expr = do
  let (tyVars, valVars, exprRemaining) = collectTyAndValBinders expr
  let suffix = concatMap (nameString . varName) valVars ++ "Ident"
  let (additionalVarTys, finalResultTy) = splitFunTys $
                                            snd $ splitForAllTys $
                                              exprType exprRemaining
  let additionalVars = map (\(ty, n) -> testArgVar (suffix ++ "Additional")
                                                   ty
                                                   n)
                           $ zip additionalVarTys [1..]
  tyVars' <- do
    tyVars' <- mapM lookupOrMutantId tyVars
    dicts <- zipWithM (\tyVar tyVar' -> varIncrementalisedDictionary
                                            incrementalisedDictionaryType
                                            tyVar
                                            (mkTyVarTy tyVar)
                                            (mkTyVarTy tyVar'))
                      tyVars
                      tyVars'
    return $ (interlace tyVars tyVars') ++ dicts
 

  valVars' <- mapM lookupOrMutantId valVars
  additionalVars' <- mapM lookupOrMutantId additionalVars
  let allValVars = valVars ++ additionalVars
  let allValVars' = valVars' ++ additionalVars'
  let valVarsWithMutants = filter (not . isArrowAppTy . varType . fst) $
                             filter (not . isRealWorld . varType . fst) $ 
                               zip allValVars allValVars'

  let includeNonMutantApps vars vars' = interlace vars vars'
  let valArgs = includeNonMutantApps allValVars allValVars'
  let additionalVarsArgs = includeNonMutantApps additionalVars
                                                additionalVars'
  finalResultTy' <- mutantType finalResultTy
  valDicts <- forM  valVarsWithMutants $ \(valVar, valVar') -> do
    dictVar <- varIncrementalisedDictionary incrementalisedDictionaryType
                                            valVar
                                            (varType valVar)
                                            (varType valVar')
    dictVal <- expIncrementalisedDictionary (Type $ varType valVar)
    return $ NonRec dictVar dictVal

  exprRemaining' <- mutantExp exprRemaining

  let identityAndReplacementTestExpr innerExpr
        | valVarsWithMutants == []
        = return innerExpr
        | otherwise
        = do
            dict <- expIncrementalisedDictionary $ Type $ finalResultTy
            allIdentityOrReplaceF <- allIdentityOrReplaceFn
            incrementalisedThingT <- incrementalisedThingTy
            incrementalisedThingD <- incrementalisedThingDataCon
            let incThings = mkListExpr incrementalisedThingT $
                              map (\(valVar, valVar', dict) ->
                                     mkApps (Var $ dataConWrapId
                                                       incrementalisedThingD)
                                            [Type $ varType valVar
                                            ,Type $ varType valVar'
                                            ,Var $ head $ bindersOf dict
                                            ,Var valVar'
                                            ]
                                  )
                                  (zip3 allValVars allValVars' valDicts)
            replace <- do
              extract <- incrementalisedReplaceExtractor
              let arg (valVar, valVar')
                   | isArrowAppTy $ varType valVar = return $ Var valVar
                   | isRealWorld (varType valVar) = return $ Var realWorldPrimId
                   | otherwise
                   = do varDict <- expIncrementalisedDictionary (Type $ varType valVar)
                        return $ mkApps (Var extract)
                                       [Type $ varType valVar
                                       ,Type $ varType valVar'
                                       ,varDict
                                       ,Var valVar']
              args <- mapM arg (zip allValVars allValVars')
              
                    
              return $ mkApps expr $ map (Type . mkTyVarTy) tyVars ++ args
              
            return $ mkApps (Var allIdentityOrReplaceF)
                            [Type finalResultTy
                            ,Type finalResultTy'
                            ,dict
                            ,incThings
                            ,replace
                            ,innerExpr
                            ]

  body <- identityAndReplacementTestExpr $
            mkApps exprRemaining' (map Var additionalVarsArgs)
            
  return $ mkLams (tyVars' ++ valArgs) $ 
             mkLets valDicts $
               body
  
isIncBoxApp exp = isJust $ splitBoxApp_maybe exp
splitBoxApp = fromJust . splitBoxApp_maybe
splitBoxApp_maybe ((isIncBox -> True) `App`
                     (Type boxResType) `App`
                     (Type boxValType) `App`
                     dataDict@(isTypeArg -> False) `App`
                     boxFun@(isTypeArg -> False) `App`
                     boxVal@(isTypeArg -> False))
  = Just (boxResType, boxValType, dataDict, boxFun, boxVal)
splitBoxApp_maybe _ = Nothing
isIncBox (Var id) = (nameString $ varName id) == "IncBox"
isIncBox _ = False
mutantIncBoxApp app = do
  let (boxResType, boxValType, dataDict, boxFun, boxVal)
        = splitBoxApp app
  boxResType' <- mutantType boxResType
  boxValType' <- mutantType boxValType
  boxVal' <- mutantExp boxVal
  incBox' <- incBoxIncCon
  applyDict <- incrementalisedDictionary
                 applicableDictionaryType
                 applicableDictionaryInstance
                 boxValType
  return $ mkApps (Var $ dataConWrapId incBox')
                  [Type boxResType, Type boxResType'
                  ,Type boxValType, Type boxValType'
                  ,applyDict, dataDict
                  ,boxFun
                  ,boxVal']

isPrimCon (Var id) = "#" `List.isSuffixOf` (nameString $ 
                                              varName id)
isPrimCon _ = False
mutantPrimCon expr arg = do let ty = exprType (App expr arg)
                            ty' <- mutantType ty
                            let replace = lookupDataConByAdd
                                            ty'
                                            AddConReplacement
                            return $ App (dataConAtType replace ty')
                                         (App expr arg)
 


mutantAlts scrutinee = liftM catMaybes . mapM (mutantAlt scrutinee)

mutantAlt :: (Expr CoreBndr, Var) -> Alt CoreBndr -> TypeLookupM (Maybe (Alt CoreBndr))
mutantAlt scrut alt@((DataAlt dataCon), binds, expr) = do
  dataCon' <- lookupMutantDataCon dataCon
  binds' <- mapM mutantId binds
  expr' <- mutantExp expr
  return $ Just $ ((DataAlt dataCon')
                  , binds'
                  , originalCaseDestructionBinds (fst scrut)
                                                 (snd scrut)
                                                 alt
                                                 expr')
mutantAlt scrut (DEFAULT, [], expr) = do
  expr' <- mutantExp expr
  return $ Just (DEFAULT, [], expr')
mutantAlt scrut _ = return Nothing -- if we handle changes-moving-into-a-value, then
                                   -- we should probably do something  here

introduceSpecialAltCases c alts = do
  b <- builderAlts' c
  return $ alts ++ b

originalCaseDestructionBinds scr_exp scr_id ((DataAlt dataCon), binds, _) exp
  = let bindsUsed id = id `elemVarSet` exprFreeIds exp
        uninc = map (\bind -> (bind, Case scr_exp
                                          scr_id
                                          (varType bind)
                                          [((DataAlt dataCon)
                                           ,binds
                                           ,Var bind)])
                  )
                  $ filter bindsUsed binds
     in case uninc of
          [] -> exp
          _  -> Let (Rec uninc) exp
original_case_destruction_binds _ _ _ exp
  = exp

builderAlts' c@(Case expr id type_ alts)
  = liftM concat $ mapM (builderAlt' c) alts

-- This is all about building a new value around an old one, for instance
-- consing an element onto the head of a list.  n is the index of the argument
-- to the constructor that is being fed the old value, so in `data List a =
-- Cons a (List a)' it would point to (List a) We require all other arguments
-- to the constructor being incrementalised - in this case, just a - to be
-- provided as part of the incrementalised_build constructor
--
-- Consider a function length (x:xs) = 1 + length xs We need to turn this into
-- something that will return an incrementalised value, where the "length xs"
-- term has gone away.  To achieve this, we incrementalise the RHS as usual,
-- but we make some assignments beforehand. We define xs_incrementalised as a
-- identity value, and elsewhere arrange for length_incrementalised to respond to
-- a identity value by making the term go away (ie, by producing another identity).
-- More work may required to make the right thing happen when more than one 
-- identity value is involved.
--
-- When we incrementalise the RHS, it will expect incrementalised values to
-- exist for all the other terms in the expression (x, in this case, although
-- it is not used in the expression). In order to make those terms available,
-- we construct replace values for each argument provided as part of the
-- incrementalised_build constructor and assign them to the incrementalised
-- name of the argument.
-- 
-- TODO: Since we're not doing the builders type-classily, we don't need this
-- crazy thing where we make a new case expr for each alt
builderAlt' c a@(DataAlt dataCon, binds, expr)
  = mapM (builderAltAtIndex' c a) 
         (builderMutantDataConIndexes dataCon)
builderAlt' c a = return []

builderAltAtIndex' (Case c_expr c_id c_type c_alts)
                   alt@(DataAlt dataCon, vars, expr)
                   builderConIndex = do
  type_' <- mutantType type_
  let builderCon = lookupDataConByBuilderIndex type_'
                                               builderConIndex
  replaceVarId <- mutantId replaceVar
  replaceVarValue <- identityValue replaceVar
  builderArgsIds <- mapM mutantId builderArgs
  builderArgsValues <- mapM replaceValue builderArgs
  expr' <- mutantExp expr
  return (DataAlt builderCon
         ,builderArgs
         ,originalCaseDestructionBinds c_expr c_id alt $
                  mkLets ((NonRec replaceVarId replaceVarValue)
                          :(map (\(id, val) -> NonRec id val)
                                (zip builderArgsIds builderArgsValues)))
                         expr')

  where type_ = dataConOrigResTy dataCon
        builderArgs = listWithout builderConIndex vars
        replaceVar = vars !! builderConIndex
        identityValue var = incrementalisedDictElement 
                           incrementalisedIdentityMk
                           (typeFor var)
        replaceValue var = liftM2 App
                                  (incrementalisedDictElement
                                     incrementalisedReplaceMk
                                     (typeFor var))
                                  (return $ Var var)

isDataConExp (Var (idDetails -> (DataConWrapId _))) = True
isDataConExp (Var (idDetails -> (DataConWorkId _))) = True
isDataConExp _ = False
 


mutantClass = id

isExprVar (Var id) = True
isExprVar _ = False

exprVar (Var id) = id
exprVar other = error ("exprVar " ++ (showSDoc $ ppr other))

typeFor :: Var -> Type
typeFor v
 | isTyVar v = mkTyVarTy v
 | otherwise = varType v

isRealWorld (splitTyConApp_maybe -> Just (tyCon, args))
   = getUnique (getName tyCon) == statePrimTyConKey
isRealWorld _ = False

collectTypeArgs expr = go expr []
  where go (App expr arg@(isTypeArg -> True)) args = go expr (arg:args)
        go expr                               args = (expr, args)
