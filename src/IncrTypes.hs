{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}
module IncrTypes where



import Control.Monad
import qualified Data.List as List
import Data.Maybe
import Debug.Trace
import Safe

import BasicTypes 
import CoreSyn
import DataCon
import HscTypes hiding (lookupDataCon, lookupType)
import IdInfo
import Literal
import MkId
import Name hiding (varName)
import OccName hiding (varName)
import qualified OccName as OccName
import Outputable
import SrcLoc
import TyCon
import Type
import TysWiredIn
import Unique
import Var

import AdditionalDataCons
import Lookups
import Names
import Utils


mutantId :: Var -> TypeLookupM Var
mutantId var = liftM (mk var') (mutantType $ varType var)
  where var' | List.isPrefixOf "$f" (occNameString $ nameOccName $ varName var)
             = mutantClsName (mutantName $ varName var) $ varType var
             | otherwise                                    
             = (mutantName $ varName var)
        mk n t | isTcTyVar var  = mkTcTyVar n t (tcTyVarDetails var)
               | isTyVar var    = mkTyVar n t
               | isGlobalId var = mkGlobalVar VanillaId n t vanillaIdInfo
               | isLocalVar var = local_mk    VanillaId n t vanillaIdInfo
        local_mk | isExportedId var  = mkExportedLocalVar
                 | otherwise         = mkLocalVar


lookupOrMutantId :: Var -> TypeLookupM Var
lookupOrMutantId var = do
  var' <- lookupMutantId_maybe var
  case var' of
    Just v    -> return v
    otherwise -> mutantId var



mutantType :: Type -> TypeLookupM Type
mutantType (getTyVar_maybe -> Just tyVar)
  = liftM mkTyVarTy $ mutantTyVar tyVar
mutantType (splitAppTy_maybe -> Just (a, b))
  | isApp a   = liftM2 mkAppTy (mutantType a) (mutantType b)
  | otherwise = liftM2 mkAppTy (mutantType a >>= return . (`mkAppTy` b))
                               (mutantType b)
  -- | otherwise = liftM2 mkAppTy (do t <- mutantType a
  --                                  return $ mkAppTy t b)
  where isApp (splitTyConApp_maybe -> Just (con, _)) = isFunTyCon con
        isApp _                                      = False

mutantType (splitFunTy_maybe -> Just (a, b))
  = liftM2 mkFunTy (mutantType a) (mutantType b)

mutantType (splitTyConApp_maybe -> Just (con, tys))
  = liftM2 mkTyConApp (lookupMutantTyCon con) (mapM mutantType tys)

mutantType (splitForAllTy_maybe -> Just (tyVar, ty)) = do
  ty' <- mutantType ty
  tyVar' <- mutantTyVar tyVar
  incDType <- incrementalisedDictionaryType (mkTyVarTy tyVar) (mkTyVarTy tyVar')
  return $ mkForAllTy tyVar $ mkForAllTy tyVar' (mkFunTy incDType ty')

mutantTyVar v = do m <- lookupOrMutantId v
                   let k = varType m
                   return $ setVarType m (duplicateKindArgs k)


mutantTyCon tyCon = do
  rhs <- mutantAlgTyConRhs tyCon $ algTyConRhs tyCon
  tyvars <- liftM (interlace (tyConTyVars tyCon))
                       (mapM mutantTyVar $ tyConTyVars tyCon)
  cls <- lookupMutantClass (fromJust $ tyConClass_maybe tyCon)
  let mutantClassTyCon = mkClassTyCon name kind tyvars rhs cls isRec
  let mutantAlgTyCon = mkAlgTyCon name kind tyvars predTys rhs parent 
                                  isRec hasGen declaredGadt
  let mutantFunTyCon = tyCon
 
  let newTyCon | isAlgTyCon tyCon      = mutantAlgTyCon
               | isAbstractTyCon tyCon = makeTyConAbstract $ mutantAlgTyCon
               | isClassTyCon tyCon    = mutantClassTyCon
               | isFunTyCon tyCon      = mutantFunTyCon
               | isPrimTyCon tyCon     = tyCon
               | otherwise = trace ("Don't know how to mutate tyCon " ++ 
                                     (showSDoc.ppr.getName$tyCon) ++
                                      " :: " ++ (showSDoc.ppr$tyCon))
                                   tyCon
  classBinds <- if hasClassBinds
                  then classBind tyCon mutantAlgTyCon >>= (\x -> return [x])
                  else return []
  return (newTyCon, classBinds)
  where
    name = mutantName $ getName tyCon
    kind = duplicateKindArgs $ tyConKind tyCon
    isRec = boolToRecFlag $ isRecursiveTyCon tyCon
    predTys = tyConStupidTheta tyCon -- can we incrementalise constraints?
    parent = tyConParent tyCon
    hasGen = tyConHasGenerics tyCon
    declaredGadt = isGadtSyntaxTyCon tyCon

    hasClassBinds = isAlgTyCon tyCon || isAbstractTyCon tyCon

-- the idea here is that a kind of * -> * should become a kind of * -> * -> *, 
-- to make room for the incrementalised versions of the type parameters.
duplicateKindArgs (splitTyConApp_maybe -> Just (tyCon, []))
  = mkTyConApp tyCon [] 
-- if we go from TyConApp to TyConApp, then we get a (-> * * *) rather than 
-- a (* -> * -> *).
-- Going from TyConApp to FunTy doesn't seem to have that problem.
duplicateKindArgs (splitTyConApp_maybe -> Just (tyCon, kinds))
  = foldl (flip mkFunTy) (head argKinds) $ (tail argKinds) ++ 
                                           argKinds ++ 
                                           [resultKind]
  --foldr mkFunTy (resultKind) $ argKinds ++ argKinds ???
  where (resultKind:(reverse -> argKinds)) = reverse kinds
duplicateKindArgs kind@(splitFunTy_maybe -> Just (arg, res))
  = kind
  -- = mkFunTy arg (mkFunTy arg $ duplicateKindArgs res)
duplicateKindArgs args = args

mutantAlgTyConRhs tyCon (DataTyCon dataCons isEnum) = do
  newTyCon <- lookupMutantTyCon tyCon
  mutantDataCons <- mapM lookupMutantDataCon dataCons
  return $ DataTyCon (mutantDataCons ++ 
                      additionalMutantDataCons tyCon newTyCon ++
                      concatMap (builderMutantDataCons tyCon newTyCon) dataCons)
                     isEnum
mutantAlgTyConRhs _ (NewTyCon dataCon rhs etadRhs co) = do
  dataCon' <- lookupMutantDataCon dataCon
  mutantCo <- maybe (return Nothing) ((fmap Just) . lookupMutantTyCon) co
  etadType <- mutantEtadType etadRhs
  type_' <- mutantType rhs
  return $ NewTyCon dataCon'
                    type_'
                    etadType
                    mutantCo
mutantAlgTyConRhs _ AbstractTyCon
  = return AbstractTyCon
mutantAlgTyConRhs _ DataFamilyTyCon
  = return DataFamilyTyCon

mutantDataCon dataCon = do
  tyCon <- lookupMutantTyCon $ dataConTyCon dataCon
  origArgTys <- mapM mutantType $ dataConOrigArgTys dataCon
  origResTy <- mutantType $ dataConOrigResTy dataCon
  univTyVars <- liftM (interlace $ dataConUnivTyVars dataCon)
                      (mapM mutantTyVar $ dataConUnivTyVars dataCon)
  exTyVars <- mapM mutantTyVar $ dataConExTyVars dataCon
  eqSpec <- map2M mutantTyVar mutantType $ dataConEqSpec dataCon
  dcIds <- liftM2 DCIds (case dataConWrapId_maybe dataCon of
                          Just m    -> do m' <- mutantId m
                                          return $ Just m'
                          otherwise -> return Nothing)
                        (mutantId $ dataConWorkId dataCon)
  return $ mkDataCon 
    (mutantName $ dataConName dataCon)
    (dataConIsInfix dataCon)
    (dataConStrictMarks dataCon)
    (map mutantName $ dataConFieldLabels dataCon)
    univTyVars
    exTyVars
    eqSpec
    (dataConDictTheta dataCon) -- dataConTheta on GHC 7.4?
    origArgTys
    origResTy
    tyCon
    (dataConStupidTheta dataCon)
    dcIds

builderMutantDataCons oldTyCon newTyCon dataCon
  = map (builderMutantDataCon oldTyCon newTyCon dataCon) indexes
  where indexes = builderMutantDataConIndexes dataCon

builderMutantDataCon oldTyCon newTyCon dataCon n
  = let tyConName = getName newTyCon
        con = mkDataCon 
                (builderMutantDataConName tyConName n)
                False                       -- is infix?
                [HsNoBang | _ <- argTypes ] -- strictness annotations
                []                          -- field lables 
                (tyConTyVars newTyCon)      -- universally quantified type vars
                []                          -- existentially quantified type vars
                []                          -- gadt equalities
                []                          -- theta type
                argTypes                    -- original argument types
                (mkTyConTy newTyCon)-- ???  -- original result type
                newTyCon                    -- representation type constructor
                []                          -- stupid theta
                (mkDataConIds (builderMutantDataConWrapId con n)
                              (builderMutantDataConWorkId con n)
                              con)
        argTypes = listWithout n (dataConOrigArgTys dataCon)
     in con
                                                   
                                              
additionalMutantDataCons oldTyCon newTyCon = map addAdditionalCon
                                                 additionalConTypes
  where addAdditionalCon type_ = additionalCon type_
        additionalCon = additionalMutantDataCon newTyCon oldTyCon
additionalMutantDataCon newTyCon oldTyCon addConType
  = let tyConName = getName newTyCon
        con = mkDataCon 
                (additionalMutantDataConName tyConName  addConType)
                False                       -- is infix?
                [HsNoBang | _ <- argTypes ] -- strictness annotations
                []                          -- field lables 
                (tyConTyVars newTyCon)      -- universally quantified type vars
                []                          -- existentially quantified type vars
                []                          -- gadt equalities
                []                          -- theta type
                argTypes                    -- original argument types
                (mkTyConTy newTyCon)-- ???  -- original result type
                newTyCon                    -- representation type constructor
                []                          -- stupid theta
                (mkDataConIds (additionalMutantDataConWrapId con addConType)
                              (additionalMutantDataConWorkId con addConType)
                              con)
        argTypes = additionalMutantDataConArgTypes oldTyCon addConType
     in  con
additionalMutantDataConName tyConName addConType
  = mutantNameIntoSpace tyConName 
                        OccName.dataName 
                        (additionalConSuffix addConType)
additionalMutantDataConWorkId dataCon addConType
  = mutantNameUnique (dataConName dataCon)
                     OccName.varName 
                     ("data_con_work_" ++ additionalConSuffix addConType)
additionalMutantDataConWrapId dataCon addConType
  = mutantNameUnique (dataConName dataCon)
                     OccName.varName 
                     ("data_con_wrap_" ++ additionalConSuffix addConType)
additionalMutantDataConReplaceVar tyCon
  = mkTyVar (mutantNameIntoSpace (getName tyCon)
                                 OccName.varName 
                                 "tyvar_for_replacement_value")
            liftedTypeKind
additionalMutantDataConArgTypes tyCon AddConReplacement
  = [funTyConToAppTy tyCon]
additionalMutantDataConArgTypes _ _ = [] 

builderMutantDataConName tyConName n
  = mutantNameIntoSpace tyConName 
                        OccName.dataName 
                        (builderConSuffix n)
builderMutantDataConWorkId dataCon n
  = mutantNameUnique (dataConName dataCon)
                     OccName.varName 
                     ("data_con_work_" ++ builderConSuffix n)
builderMutantDataConWrapId dataCon n
  = mutantNameUnique (dataConName dataCon)
                     OccName.varName 
                     ("data_con_wrap_" ++ builderConSuffix n)


  
funTyConToAppTy tyCon = mkAppTys (mkTyConTy tyCon) 
                                 (map mkTyVarTy $ tyConTyVars tyCon)

mutantEtadType (tyVars, type_) = liftM2 (,)
                                         (mapM mutantTyVar tyVars)
                                         (mutantType type_)






incrementalisedDictElement getter ty = do
  ty' <- mutantType ty
  g <- getter
  dict <- expIncrementalisedDictionary $ Type ty
  return $ mkApps (Var g)
                  [Type ty
                  ,Type ty'
                  ,dict]

varIncrementalisedDictionary :: Var -> Type -> Type -> TypeLookupM Var
varIncrementalisedDictionary var ty ty' = do
  t <- incrementalisedDictionaryType ty ty'
  return $ mkLocalVar VanillaId
                    (mutantNameIntoSpace (varName var)
                                      OccName.varName
                                      "incrementalisedD")
                    t
                    vanillaIdInfo

expIncrementalisedDictionary (Var v) = do
  let ty = varType v
  ty' <- mutantType ty
  liftM Var $ varIncrementalisedDictionary v ty ty'
expIncrementalisedDictionary (Type t@(isTyVarTy -> True)) = do
  ty' <- mutantType t
  liftM Var $ varIncrementalisedDictionary 
                (getTyVar "shouldn't ever happen" t)
                t
                ty'
expIncrementalisedDictionary (Type t) = do
  let base_t = snd $ splitFunTys $ snd $ splitForAllTys t
  inst <- incrementalisedDictionaryInstance base_t
  let args = case splitTyConApp_maybe t of
               Just (_, a) -> a
               Nothing     -> []
  argsMutants <- mapM mutantType args
  argsInsts <- mapM expIncrementalisedDictionary (map Type args)
  return $ mkApps (Var $ inst) $ interlace3 (map Type args)
                                            (map Type argsMutants)
                                            argsInsts

incrementalisedDictionaryInstance :: Type -> TypeLookupM Var
incrementalisedDictionaryInstance type_
  | let tyCon = fmap fst $ splitTyConApp_maybe type_
     in isJust tyCon && (isPrimTyCon $ fromJust tyCon)
  = lookupPreludeFn "" "undefined"
  | isTyVarTy type_
  = varIncrementalisedDictionary (getTyVar "won't happen" type_)
                                 type_
                             =<< (mutantType type_)
                                      
  | otherwise
  = liftM (tyThingId . fromJustNote ("incrementalisedDictionaryInstance for " ++
                                     (showSDoc $ ppr type_) ++ " " ++
                                     (showSDoc $ ppr mName)))
          (lookupTypeM (fmap adaptModule $ nameModule_maybe oldName)
                       (lookupNameEnvString (occNameString mName)
                                            (occNameSpace mName)))
  where (oldName, mName) = incrementalisedDictionaryInstanceName
                                (OccName.varName)
                                type_

clsOccName space clsName params  = mkOccName space newName
  where
    newName = "$f" ++ clsName ++ 
              concat paramNames'
    paramNames = map (nameString . getName) params
    paramNames' = interlace paramNames (map (++"_incrementalised") paramNames)

mutantClsName name instanceType
  = sameSortOfName name
                   (mutantClsOccName (occNameSpace $ nameOccName name)
                                     instanceType)
                   (nameModule name)

mutantClsOccName nameSpace instanceType = clsOccName nameSpace clsName' params
  where 
    (_, cls) = splitFunTys $ snd $ splitForAllTys instanceType
    (clsTy, paramsTys) = fromJustNote ("mutantClsOccName couldn't split " ++
                                       (showSDoc $ ppr cls) ++ 
                                       "(was " ++ 
                                       (showSDoc $ ppr $ instanceType) ++
                                       ")")
                         $ splitTyConApp_maybe cls
    baseName = getName $ clsTy
    params = map (\t -> maybe (error $ "mutantClsOccName couldn't split " ++
                                       (showSDoc $ ppr cls) ++
                                       " at arg " ++
                                       (showSDoc $ ppr t))
                              fst
                              (splitTyConApp_maybe t))
                 paramsTys
    clsNameString = nameString $ baseName
    clsName' = (drop 2 $ clsNameString) ++ "_incrementalised"

incrementalisedDictionaryInstanceName nameSpace instanceType
  = (baseName, mkOccName nameSpace $
                         "$fIncrementalised" ++ 
                         (nameString baseName) ++ 
                         (adaptName baseName) ++
                         "_incrementalised")
  where 
    (clsTy, _) = fromJustNote ("incrementalisedDictionaryInstanceName"++
                                       " couldn't split " ++
                                       (showSDoc $ ppr instanceType) ++
                                       ")")
                               $ splitTyConApp_maybe instanceType
    baseName = getName $ clsTy


classBind :: TyCon -> TyCon -> TypeLookupM (CoreBind)
classBind tyCon mutantTyCon = do
  classDataCon <-  fmap (tyThingDataCon . fromJust) $ 
                        lookupInctimeTyThing "D:Incrementalised"
                                             OccName.dataName

  let classDataConExp = mkApps (Var $ dataConWrapId classDataCon)
                               (map Type [baseType, incrementalisedType])
  let classInstanceId
        = mkGlobalVar VanillaId
                      classInstanceName
                      (mkForAllTys (tyConTyVars mutantTyCon)
                                   (mkTyConApp (dataConTyCon classDataCon)
                                               [baseType
                                               ,incrementalisedType]))
                      vanillaIdInfo
  let mkTestAlts f alts
        = Lam (testVar 0) $
            f $
              Case (Var $ testVar 0)
                   (testVar 0)
                   (mkTyConTy boolTyCon)
                   ((DEFAULT, [], Var falseDataConId):alts)
  let mkTest addConType argTypes
        = mkTestAlts id $
                      [(DataAlt $ lookupDataConByAdd (mkTyConTy mutantTyCon)
                                                     addConType
                      , testArgVars argTypes
                      , Var trueDataConId)]
  let isReplace = mkTest AddConReplacement [baseType] 
  let isHoist = mkTest AddConHoist []
  dataCons_builderCons <- do
    let dataCons_indexes :: [(DataCon, Int)]
          = concat $ map (\dataCon -> 
                            zip (repeat dataCon)
                                (builderMutantDataConIndexes dataCon)
                         )
                         (tyConDataCons tyCon)
    let dataCons_builderCons :: [(DataCon, Int, DataCon)]
          = map (\(dataCon, index) ->
                   (dataCon
                   ,index
                   ,lookupDataConByBuilderIndex (mkTyConTy mutantTyCon)
                                                index))
                dataCons_indexes
    return dataCons_builderCons
  preludeEqTest <- lookupPreludeFn "GHC.Base" "eqInt"
  let varEqualsInt v i
        = mkApps (Var preludeEqTest)
                 [Var v
                 ,App (Var $ dataConWrapId intDataCon)
                      (Lit $ mkMachInt $ fromIntegral i)]

  let isIdentity = mkTest AddConIdentity []

  let mkBuilder addConType argTypes
        = mkLams (testArgVars argTypes)
                 (mkApps (dataConValueByType addConType (mkTyConTy mutantTyCon))
                         (map (Type . mkTyVarTy) (tyConTyVars mutantTyCon) ++
                          map Var (testArgVars argTypes)))

  let mkReplace = mkBuilder AddConReplacement [baseType]
  let mkIdentity = mkBuilder AddConIdentity []
  let mkHoist = mkBuilder AddConHoist []

  callUndefined <- liftM Var $ lookupPreludeFn "" "undefined"

  extractReplace <- do
    let buildValueVar = head $ testArgVars [baseType]
    return $ Lam (testVar 0) $
               Case (Var $ testVar 0)
                    (testVar 0)
                    baseType
                    ((DEFAULT, [], App callUndefined 
                                       (Type baseType)):
                     (DataAlt (lookupDataConByAdd (mkTyConTy mutantTyCon)
                                                   AddConReplacement)
                     ,[buildValueVar]
                     ,Var $ buildValueVar
                     ):[])

  return $ NonRec classInstanceId $ 
                mkLams (tyConTyVars mutantTyCon)
                       (mkApps classDataConExp [ isReplace
                                               , isHoist
                                               , isIdentity
                                               , mkReplace
                                               , mkIdentity
                                               , mkHoist
                                               , extractReplace])
  where classInstanceName = sameSortOfName
                              (getName tyCon)
                              (clsOccName OccName.varName
                                          ("Incrementalised" ++
                                           (nameString $ getName tyCon) ++
                                           (nameString $ getName tyCon) ++
                                           "_incrementalised")
                                          ([] :: [TyCon]))
                              (nameModule $ getName tyCon)
        baseName = (nameString $ classInstanceName)
        testVar = testArgVar baseName incrementalisedType
        testArgVars types = zipWith (testArgVar baseName) types [1..]
        baseType = mkTyConApp tyCon 
                               (map mkTyVarTy $ tyConTyVars tyCon)
        incrementalisedType
          = mkTyConApp mutantTyCon
                       (map mkTyVarTy $ tyConTyVars mutantTyCon)

        dataConValueByType addConType mType = 
          let con = lookupDataConByAdd mType addConType
           in dataConAtType con mType




testArgVar baseName type_ n = mkLocalVar VanillaId 
                                (mkInternalName (getUnique $ tvn) 
                                                tvn
                                                noSrcSpan)
                                type_
                                vanillaIdInfo
  where tvn = testVarOccName n baseName
testVarOccName n baseName = mkOccName OccName.varName
                                      (baseName ++ "_test" ++ show n)



