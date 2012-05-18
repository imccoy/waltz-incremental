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
import InstEnv (Instance (..), roughMatchTcs)
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
import VarSet (mkVarSet)

import AdditionalDataCons
import Lookups
import Names
import Utils


mutantId :: Var -> TypeLookupM Var
mutantId var = mk var' =<< (mutantType $ varType var)
  where var' | List.isPrefixOf "$f" (occNameString $ nameOccName $ varName var)
             = mutantClsName (mutantName $ varName var) $ varType var
             | otherwise                                    
             = (mutantName $ varName var)
        mk :: Name -> Type -> TypeLookupM Var
        mk n t | isTcTyVar var          = return $ mkTcTyVar n t 
                                                             (tcTyVarDetails var)
               | isTyVar var            = return $ mkTyVar n t
               | isGlobalId var         = buildExpId mkGlobalVar
               | isExportedLocalVar var = buildExpId mkExportedLocalVar
               | isLocalVar var         = buildExpId mkLocalVar
               where buildExpId mk' = liftM4 mk'
                                             (details var)
                                             (return n)
                                             (return t)
                                             (return $ idInfo var)
        details var = mutantIdDetails (idDetails var)
        isExportedLocalVar v = isExportedId v && isLocalVar v

mutantIdDetails VanillaId = return VanillaId
mutantIdDetails (RecSelId tyCon naughty) = liftM2 RecSelId
                                                  (lookupMutantTyCon tyCon)
                                                  (return naughty)
mutantIdDetails (DataConWorkId dataCon) = liftM DataConWorkId
                                                (lookupMutantDataCon dataCon)
mutantIdDetails (DataConWrapId dataCon) = liftM DataConWrapId
                                                (lookupMutantDataCon dataCon)
mutantIdDetails (ClassOpId cls) = liftM ClassOpId
                                        (lookupMutantClass cls)
mutantIdDetails details@(PrimOpId _) = return details
mutantIdDetails details@(FCallId _) = return details
mutantIdDetails details@(TickBoxOpId _) = return details
mutantIdDetails details@(DFunId _ _) = return details
                                                         

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
  applicable <- applicableClassBind tyCon mutantAlgTyCon
  classBinds <- if hasClassBinds
                  then sequence [incrementalisedClassBind tyCon mutantAlgTyCon
                                ,return applicable]
                  else return []
  instances <- if hasClassBinds
                  then sequence [tcInstanceApplicable tyCon
                                                      mutantAlgTyCon
                                                      applicable]
                  else return []
  return (newTyCon, classBinds, instances)
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
                     False
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
  wrapper' <- case dataConWrapId_maybe dataCon of
                          Just m    -> mutantId m >>= return . Just
                          otherwise -> return Nothing
  worker' <- mutantId $ dataConWorkId dataCon
  let dcIds = DCIds wrapper' worker'
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
                     OccName.dataName 
                     ("data_con_work_" ++ builderConSuffix n)
builderMutantDataConWrapId dataCon n
  = mutantNameUnique (dataConName dataCon)
                     OccName.dataName 
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

varIncrementalisedDictionary :: (Type -> Type -> TypeLookupM Type) ->
                                Var ->
                                Type ->
                                Type ->
                                TypeLookupM Var
varIncrementalisedDictionary dictTy var ty ty' = do
  t <- dictTy ty ty'
  varIncrementalisedDictionaryTy t var

varIncrementalisedDictionaryTy t var = 
  return $ mkLocalVar VanillaId
                    (mutantNameIntoSpace (varName var)
                                      OccName.varName
                                      "incrementalisedD")
                    t
                    vanillaIdInfo

expIncrementalisedDictionary (Var v) = do
  let ty = varType v
  ty' <- mutantType ty
  liftM Var $ varIncrementalisedDictionary
                incrementalisedDictionaryType
                v
                ty
                ty'
expIncrementalisedDictionary (Type t)
  = incrementalisedDictionary incrementalisedDictionaryType
                              incrementalisedDictionaryInstance
                              t

incrementalisedDictionary dictTy _ t@(getTyVar_maybe -> (Just tyVar)) = do
  ty' <- mutantType t
  liftM Var $ varIncrementalisedDictionary
                dictTy
                tyVar
                t
                ty'
incrementalisedDictionary dictTy dictVal t = do
  let base_t = snd $ splitFunTys $ snd $ splitForAllTys t
  inst <- dictVal base_t
  
  includeArgs <- do
    incBox <- incBoxTyCon
    return $ tyConAppTyCon base_t /= incBox
  
  allArgs <- do
    let args = case splitTyConApp_maybe t of
                 Just (_, a) -> a
                 Nothing     -> []
    argsMutants <- mapM mutantType args
    argsInsts <- mapM (incrementalisedDictionary dictTy dictVal) args
    if includeArgs
      then return $ interlace3 (map Type args)
                               (map Type argsMutants)
                               argsInsts
      else return $ interlace  (map Type args)
                               (map Type argsMutants)

  return $ mkApps (Var $ inst) $ allArgs
dictionaryInstance :: Type -> (NameSpace -> Type -> (Name, OccName)) -> TypeLookupM Var
dictionaryInstance type_ nameFinder
  | let tyCon = fmap fst $ splitTyConApp_maybe type_
     in isJust tyCon && (isPrimTyCon $ fromJust tyCon)
  = lookupPreludeFn "" "undefined"
  | isTyVarTy type_
  = varIncrementalisedDictionary incrementalisedDictionaryType
                                 (getTyVar "won't happen" type_)
                                 type_
                             =<< (mutantType type_)
                                      
  | otherwise
  = liftM (tyThingId . fromJustNote ("incrementalisedDictionaryInstance for " ++
                                     (showSDoc $ ppr type_) ++ " " ++
                                     (showSDoc $ ppr mName)))
          (lookupTypeM (fmap adaptModule $ nameModule_maybe oldName)
                       (lookupNameEnvString (occNameString mName)
                                            (occNameSpace mName)))
  where (oldName, mName) = nameFinder
                                (OccName.varName)
                                type_

incrementalisedDictionaryInstance :: Type -> TypeLookupM Var
incrementalisedDictionaryInstance type_
  = dictionaryInstance type_ incrementalisedDictionaryInstanceName

applicableDictionaryInstance :: Type -> TypeLookupM Var
applicableDictionaryInstance type_
  = dictionaryInstance type_ applicableDictionaryInstanceName

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
  = dictionaryInstanceName nameSpace instanceType "Incrementalised"

applicableDictionaryInstanceName nameSpace instanceType
  = dictionaryInstanceName nameSpace instanceType "ApplicableIncrementalised"

dictionaryInstanceName nameSpace instanceType className
  = (baseName, mkOccName nameSpace $
                         "$f" ++ className ++ 
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

makeClassInstance clsNameS tyCon mutantTyCon args fns = do

  classDataCon <-  fmap (tyThingDataCon . fromJust) $ 
                        lookupInctimeTyThing ("D:" ++ clsNameS)
                                             OccName.dataName

  let classDataConExp = mkApps (Var $ dataConWrapId classDataCon)
                               (map Type [baseType, incrementalisedType])
  let instanceName = sameSortOfName
                              (getName tyCon)
                              (clsOccName OccName.varName
                                          (clsNameS ++
                                           (nameString $ getName tyCon) ++
                                           (nameString $ getName tyCon) ++
                                           "_incrementalised")
                                          ([] :: [TyCon]))
                              (nameModule $ getName tyCon)
        
  cls <- fmap (tyThingClass . fromJust) $
              lookupInctimeTyThing clsNameS OccName.tcName
  let classInstanceId
        = mkGlobalVar (DFunId 0 True)
                      instanceName
                      (mkForAllTys (tyConTyVars mutantTyCon)
                                   (mkFunTys (map varType args)
                                             (mkPredTy $ ClassP
                                               cls
                                               [baseType
                                               ,incrementalisedType])))
                      vanillaIdInfo
  
  return $ NonRec classInstanceId $ 
                mkLams (tyConTyVars mutantTyCon ++ args)
                       (mkApps classDataConExp fns)
  where baseType = mkTyConAppTy tyCon 
        incrementalisedType = mkTyConAppTy mutantTyCon

incrementalisedClassBind :: TyCon -> TyCon -> TypeLookupM (CoreBind)
incrementalisedClassBind tyCon mutantTyCon = do
  let mkTestAlts n f alts
        = Lam (testVar n) $
            f $
              Case (Var $ testVar n)
                   (testVarC n)
                   (mkTyConTy boolTyCon)
                   ((DEFAULT, [], Var falseDataConId):alts)
  let mkTest n addConType argTypes
        = mkTestAlts n id $
                      [(DataAlt $ lookupDataConByAdd (mkTyConTy mutantTyCon)
                                                     addConType
                      , testArgVars baseName argTypes
                      , Var trueDataConId)]
  let isReplace = mkTest 0 AddConReplacement [baseType] 
  let isIdentity = mkTest 1 AddConIdentity []

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

  let mkBuilder addConType argTypes
        = mkLams (testArgVars baseName argTypes)
                 (mkApps (dataConValueByType addConType (mkTyConTy mutantTyCon))
                         (map (Type . mkTyVarTy) (tyConTyVars mutantTyCon) ++
                          map Var (testArgVars baseName argTypes)))

  let mkReplace = mkBuilder AddConReplacement [baseType]
  let mkIdentity = mkBuilder AddConIdentity []

  callUndefined <- liftM Var $ lookupPreludeFn "" "undefined"

  extractReplace <- do
    let buildValueVar = head $ testArgVars baseName [baseType]
    return $ Lam (testVar 3) $
               Case (Var $ testVar 3)
                    (testVarC 3)
                    baseType
                    ((DEFAULT, [], App callUndefined 
                                       (Type baseType)):
                     (DataAlt (lookupDataConByAdd (mkTyConTy mutantTyCon)
                                                   AddConReplacement)
                     ,[buildValueVar]
                     ,Var $ buildValueVar
                     ):[])

  makeClassInstance "Incrementalised"
                    tyCon
                    mutantTyCon
                    []
                    [ isReplace
                    , isIdentity
                    , mkReplace
                    , mkIdentity
                    , extractReplace]
  where baseName = "Incrementalised" ++ (nameString $ getName $ tyCon)
        testVar = testArgVar baseName incrementalisedType
        testVarC = testArgVar (baseName ++ "C") incrementalisedType
        
        baseType = mkTyConAppTy tyCon 
        incrementalisedType = mkTyConAppTy mutantTyCon

        dataConValueByType addConType mType
           = dataConAtType (lookupDataConByAdd mType addConType)
                           mType

mkTyConAppTy tyCon = mkTyConApp tyCon
                                (map mkTyVarTy $ tyConTyVars tyCon)

applicableClassBind :: TyCon -> TyCon -> TypeLookupM CoreBind
applicableClassBind tyCon mutantTyCon = do
  -- class ApplicableIncrementalised base incrementalised where 
  --   applyInputChange  :: incrementalised -> base -> base

  dataConAlts <- mapM (\dataCon -> do
                        mutantDataCon <- lookupMutantDataCon dataCon
                        let initialArgTys = dataConOrigArgTys dataCon
                        let mutantArgTys = dataConOrigArgTys mutantDataCon

                        let initialVars = testArgVars (baseName ++ "Initial")
                                                      (initialArgTys)
                        let mutantVars = testArgVars (baseName ++ "Mutant")
                                                     (mutantArgTys)

                        applier <- inputChangeApplier
                        applicableDict <- mapM (incrementalisedDictionary
                                                  applicableDictionaryType
                                                  applicableDictionaryInstance)
                                               initialArgTys
                                               
                        let recursiveApply
                             = mkApps (Var $ dataConWrapId dataCon)
                                      (map (Type . mkTyVarTy)
                                           (tyConTyVars tyCon) ++
                                       map (\(initialVar, mutantVar, dict) ->
                                              mkApps (Var applier)
                                                     [Type $ varType initialVar
                                                     ,Type $ varType mutantVar
                                                     ,dict
                                                     ,Var $ mutantVar
                                                     ,Var $ initialVar])
                                           (zip3 initialVars 
                                                 mutantVars 
                                                 applicableDict))
                        return (DataAlt mutantDataCon
                               ,mutantVars
                               ,Case (Var initialVar)
                                     initialVarC
                                     baseType
                                     [(DataAlt dataCon
                                      ,initialVars
                                      ,recursiveApply)
                                     ])
                      )
                      (tyConDataCons tyCon)

  undefinedFn <- lookupPreludeFn "" "undefined"

  let fn = Lam changeVar $
             Lam initialVar $
               Case (Var changeVar)
                    changeVarC
                    baseType
                    ([(DEFAULT, [], (App (Var undefinedFn) (Type baseType)))
                      ] ++dataConAlts ++
                      [(DataAlt $ lookupDataConByAdd (mkTyConTy mutantTyCon)
                                                     AddConReplacement
                        ,[replaceVar]
                        ,Var replaceVar
                       ),
                       (DataAlt $ lookupDataConByAdd (mkTyConTy mutantTyCon)
                                                     AddConIdentity
                        ,[]
                        ,Var initialVar)])

  dictArgs <- mapM (\var -> do
                      let ty = mkTyVarTy $ var
                      ty' <- mutantType ty
                      cls <- lookupTypeClassString "ApplicableIncrementalised"
                      let t = mkPredTy $ ClassP cls [ty, ty']
                      varIncrementalisedDictionaryTy t var)
                   (tyConTyVars tyCon)
                    
  makeClassInstance "ApplicableIncrementalised"
                    tyCon
                    mutantTyCon
                    dictArgs
                    [fn]
  where baseType = mkTyConAppTy tyCon 
        incrementalisedType = mkTyConAppTy mutantTyCon
        baseName = "applicableIncrementalised" ++ (nameString $ getName tyCon)
        changeVar = testArgVar (baseName ++ "Change") incrementalisedType 0
        changeVarC = testArgVar (baseName ++ "ChangeC") incrementalisedType 0
        initialVar = testArgVar (baseName ++ "Initial") baseType 0
        initialVarC = testArgVar (baseName ++ "InitialC") baseType 0
        replaceVar = testArgVar (baseName ++ "Replace") baseType 0
        boolVar = testArgVar (baseName ++ "Test") boolTy 0
 
tcInstanceApplicable tyCon mutantTyCon dictbind = do
  is_cls <- lookupTypeClassString "ApplicableIncrementalised"
                >>= return . getName
  let is_tys = [mkTyConAppTy tyCon, mkTyConAppTy mutantTyCon]
  let is_tcs = roughMatchTcs is_tys
  let is_tvs = mkVarSet $ tyConTyVars mutantTyCon
  let is_dfun = case dictbind of
                  NonRec id _ -> id
  let is_flag = NoOverlap
  return $ Instance is_cls is_tcs is_tvs is_tys is_dfun is_flag


testArgVars baseName types = zipWith (testArgVar baseName) types [1..]
testArgVar baseName type_ n = mkLocalVar VanillaId 
                                (mkInternalName (getUnique $ tvn) 
                                                tvn
                                                noSrcSpan)
                                type_
                                vanillaIdInfo
  where tvn = testVarOccName n baseName
testVarOccName n baseName = mkOccName OccName.varName
                                      (baseName ++ "_test" ++ show n)



