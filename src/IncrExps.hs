{-# LANGUAGE ViewPatterns, TupleSections, ScopedTypeVariables, DoRec #-}

module IncrExps where

import Control.Monad
import qualified Data.List as List
import Data.Maybe


import GHC (getName)
import CoreSyn
import DataCon
import Id
import IdInfo (IdDetails (DataConWrapId, DataConWorkId), isDeadOcc, occInfo)
import Outputable
import PrelNames (statePrimTyConKey)
import Type
import TysWiredIn
import Unique (getUnique)
import Var



import AdditionalDataCons
import Lookups
import IncrTypes
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
mutantExp (Var id) = liftM Var $ lookupOrMutantId id

mutantExp app@(App expr arg)
  = monadGuard [
      (return $ isIncBoxApp app ,mutantIncBoxApp app)
     ,(isNoIncApp app           ,mutantNoIncApp app)
     ,(isNoIncLam app           ,mutantNoIncLam app)
     ,(return $ isPrimCon expr  ,mutantPrimCon expr arg)
     ,(return $ isTypeArg arg && (isRealWorld $ exprType arg), do
         expr' <- mutantExp expr
         return $ App expr' arg
        )
     ,(return $ isTypeArg arg && isDataConApp expr arg, do
         expr' <- mutantExp expr
         arg' <- mutantExp arg
         return $ App (App expr' arg) arg'
        )
     ,(return $ isTypeArg arg, do
         expr' <- mutantExp expr
         arg' <- mutantExp arg
         argD <- expIncrementalisedDictionary arg
         return $ App (App (App expr' arg) arg') argD)
     ,(return True              ,liftM2 App (mutantExp expr) (mutantExp arg))]
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
                                                 mutantAlts alts)
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
  tyVars' <- liftM concat $ forM tyVars $ \tyVar -> do
    id' <- lookupOrMutantId tyVar
    sequence [return tyVar
             ,return id'
             ,varIncrementalisedDictionary incrementalisedDictionaryType
                                           tyVar
                                           (mkTyVarTy tyVar)
                                           (mkTyVarTy id')]
  valVars' <- mapM lookupOrMutantId valVars
  additionalVars' <- mapM lookupOrMutantId additionalVars
  let allValVars = valVars ++ additionalVars
  let allValVars' = valVars' ++ additionalVars'
  let valVarsWithMutants = filter (not . isFunTy . varType . fst) $
                             filter (not . isRealWorld . varType . fst) $ 
                               zip allValVars allValVars'

  finalResultTy' <- mutantType finalResultTy
  valDicts <- forM  valVarsWithMutants $ \(valVar, valVar') -> do
    dictVar <- varIncrementalisedDictionary incrementalisedDictionaryType
                                            valVar
                                            (varType valVar)
                                            (varType valVar')
    dictVal <- expIncrementalisedDictionary (Type $ varType valVar)
    return $ NonRec dictVar dictVal

  scrutinee <- do
    tests <- forM valVarsWithMutants $ \(valVar, valVar') -> do
      ts <- incrementalisedIdentityTest
      dict <- varIncrementalisedDictionary incrementalisedDictionaryType
                                           valVar
                                           (typeFor valVar)
                                           (typeFor valVar')
      return $ mkApps (Var ts) [ Type (varType valVar)
                               , Type (varType valVar')
                               , Var dict
                               , Var valVar']

    and <- lookupPreludeFn "" "&&"
    return $ foldl (\a b -> mkApps (Var and) [a,b]) 
                   (Var $ dataConWrapId trueDataCon)
                   tests
      

  exprRemaining' <- mutantExp exprRemaining

  mkIdentity <- do
    mk <- incrementalisedIdentityMk
    dict <- expIncrementalisedDictionary $ Type $ finalResultTy
    return $ mkApps (Var mk) [ Type finalResultTy
                             , Type finalResultTy'
                             , dict]

  return $ mkLams (tyVars' ++ allValVars') $ 
             mkLets valDicts $
               Case scrutinee
                    (testArgVar suffix
                                (mkTyConTy boolTyCon)
                                0)
                    finalResultTy'
                    [(DataAlt falseDataCon
                     ,[]
                     , mkApps exprRemaining' (map Var additionalVars'))
                    ,(DataAlt trueDataCon, [], mkIdentity)]
  
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

isNoIncApp exp = fmap isJust $ splitNoIncApp_maybe exp
splitNoIncApp exp = fmap fromJust $ splitNoIncApp_maybe exp
splitNoIncApp_maybe app = do
  noIncAppUniq <- liftM idUnique noIncAppId
  let isIncAppVar (Var v) = idUnique v == noIncAppUniq
      isIncAppVar _       = False
  case app of
   ((isIncAppVar -> True) `App` 
      (Type fType) `App`
      (Type argType) `App`
      f@(isTypeArg -> False) `App`
      arg@(isTypeArg -> False)) -> return $ Just (fType, argType
                                                 ,f, arg)
   otherwise                    -> return Nothing
mutantNoIncApp app = do
  (fType, argType, f, arg) <- splitNoIncApp app
  f' <- mutantExp f -- this must be an id or a lam that has a 
                    -- matching NoIncLam
  arg' <- mutantExp arg
  return $ f' `App` arg `App` arg'


isNoIncLam expr = liftM isJust $ splitNoIncLam_maybe expr
splitNoIncLam_maybe exp = do
  noIncLamUniq <- liftM idUnique noIncLamId
  let isNoIncLamVar (Var v) = idUnique v == noIncLamUniq
      isNoIncLamVar _       = False
  case exp of
    ((isNoIncLamVar -> True) `App` 
      (Type fTy) `App`
      (Type resultTy) `App`
      (Lam argBind fBody)) -> return $ Just (argBind, resultTy, fBody)
    otherwise -> return Nothing
splitNoIncLam expr = liftM fromJust $ splitNoIncLam_maybe expr

mutantNoIncLam expr = do
  (argBind, resultTy, fBody) <- splitNoIncLam expr
  fBody' <- mutantExp fBody
  argBind' <- mutantId argBind
  return $ Lam argBind $
             Lam argBind' $
               fBody'

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
 


mutantAlts = liftM catMaybes . mapM mutantAlt

mutantAlt :: Alt CoreBndr -> TypeLookupM (Maybe (Alt CoreBndr))
mutantAlt (DataAlt _, [], _) = return Nothing -- handled by replaceAlt
mutantAlt ((DataAlt dataCon), binds, expr) = do
  dataCon' <- lookupMutantDataCon dataCon
  binds' <- mapM mutantId binds
  expr' <- mutantExp expr
  return $ Just $ ((DataAlt dataCon'), binds', expr')
mutantAlt (DEFAULT, [], expr) = do
  expr' <- mutantExp expr
  return $ Just (DEFAULT, [], expr')
mutantAlt _ = return Nothing -- if we handle changes-moving-into-a-value, then
                             -- we should probably do something  here

introduceSpecialAltCases c alts = do
  let (defaultAlts, nonDefaultAlts)
        = List.partition (\(a, _, _) -> a == DEFAULT) alts
  r <- replaceAlt' c defaultAlts
  b <- builderAlts' c
  return $ [(DEFAULT, [], r)] ++ nonDefaultAlts ++ b


replaceAlt' c@(Case expr id type_ alts) defaultAlts = do
  destType <- mutantType type_
  srcType  <- mutantType $ exprType expr
  expr' <- mutantExp expr
  dict <- expIncrementalisedDictionary (Type $ exprType expr)

  replaceTest <- do
    t <- incrementalisedReplaceTest
    return $ mkApps (Var t) [Type $ exprType expr
                            ,Type srcType
                            ,dict]
  replaceExtractor <- do
    t <- incrementalisedReplaceExtractor
    return $ mkApps (Var t) [Type $ exprType expr
                            ,Type srcType
                            ,dict]
  replaceExp <- do
    mkReplace <- incrementalisedReplaceMk
    destDict <- expIncrementalisedDictionary (Type type_)
    let binders = (if isExprVar expr then [exprVar expr] else []) ++
                  (if (not . isDeadOcc . occInfo . idInfo) id
                      || (not . isExprVar) expr then [id] else [])
    let trueBinder = head binders
    let replaceLet = NonRec trueBinder $ App replaceExtractor expr'
    let replaceLets = replaceLet:(map (`NonRec` (Var trueBinder))
                                      (tail binders))
    return $ mkLets replaceLets $
               mkApps (Var mkReplace)
                      [Type type_
                      ,Type destType
                      ,destDict
                      ,c]
  return $ Case (App replaceTest expr')
                (testArgVar "replaceAlt" (mkTyConTy boolTyCon) 0)
                destType
                (defaultAlts ++
                [(DataAlt trueDataCon
                 ,[]
                 ,replaceExp)]
                )


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
                   (DataAlt dataCon, vars, expr)
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
         ,mkLets ((NonRec replaceVarId replaceVarValue)
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

isDataConApp (Var (idDetails -> (DataConWrapId _))) _ = True
isDataConApp (Var (idDetails -> (DataConWorkId _))) _ = True
isDataConApp (App exp' arg') (isTypeArg -> True) = isDataConApp exp'
                                                                arg'
isDataConApp _ _ = False
 


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


