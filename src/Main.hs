{-# LANGUAGE ViewPatterns, DoRec #-}

module Main where

import Control.Monad
import Control.Monad.Reader hiding (liftIO)
import qualified Data.List as List
import Data.Maybe
import Debug.Trace
import System.Environment (getArgs)
import System.Exit

import Outputable

import Bag
import BasicTypes 
import GHC.Paths ( libdir )
import CoreLint
import CoreMonad
import CoreSyn
import DataCon
import ErrUtils
import HscTypes hiding (lookupDataCon, lookupType)
import HscMain
import IdInfo
import Literal
import MkId
import Module
import Name hiding (varName)
import NameEnv
import OccName hiding (varName)
import qualified OccName as OccName
import StaticFlags
import Type
import TyCon
import Unique
import UniqFM
import Var

import DynFlags

import GHC hiding (exprType)


interlace :: [a] -> [a] -> [a]
interlace [] [] = []
interlace (a:as) (b:bs) = a:b:(interlace as bs)

map2 :: (a -> c) -> (b -> d) -> [(a, b)] -> [(c, d)]
map2 f g = map (\(a, b) -> (f a, g b))


map2M :: Monad m => (a -> m c) -> (b -> m d) -> [(a, b)] -> m [(c, d)]
map2M f g = mapM (\(a, b) -> do c <- f a
                                d <- g b
                                return (c, d)) 

listWithout n list = let (before, after) = splitAt n list
                      in before ++ (tail after)

instance MonadFix Ghc where
  -- stolen from ghc source
  mfix f = Ghc $ \s -> mfix (\x -> unGhc (f x) s)

type TypeLookupM a = Reader (TypeEnv, HomePackageTable, PackageTypeEnv, DynFlags) a

withTypeLookups :: TypeEnv -> TypeLookupM a -> Ghc a
withTypeLookups typeEnv f = do
  session <- getSession
  eps <- liftIO $ hscEPS session -- ExternalPackageState
  let hpt = hsc_HPT session
  let dynflags = hsc_dflags session
  return $ runReader f (typeEnv, hpt, eps_PTE eps, dynflags)

lookupMutantTyCon tyCon
    -- TODO: less manky test
  | "(->)" == (nameString.getName) tyCon = return tyCon
  | "*" == (nameString.getName) tyCon    = return tyCon
  | otherwise                            = do
    tyThing <- lookupMutantTyThing (ATyCon tyCon)
    case tyThing of
      Just (ATyCon tyCon) -> return tyCon
      otherwise           -> error $ "Got not-a-tycon for mutant '" ++ 
                                     (showSDoc $ ppr tyCon) ++ "'"

lookupMutantDataCon dataCon = do
  tyThing <- lookupMutantTyThing (ADataCon dataCon)
  case tyThing of
    Just (ADataCon dataCon) -> return dataCon
    otherwise               -> error $ "Got not-a-dataCon for mutant '" ++ 
                                       (showSDoc $ ppr dataCon) ++ "'"

lookupMutantClass cls = do
  tyThing <- lookupMutantTyThing (AClass cls)
  case tyThing of
    Just (AClass cls) -> return cls
    otherwise         -> error $ "Got not-a-class for mutant '" ++ 
                                 (showSDoc $ ppr cls) ++ "'"

lookupMutantId_maybe id = do
  tyThing <- lookupMutantTyThing (AnId id)
  case tyThing of
    Just (AnId id) -> return $ Just id
    otherwise      -> return $ Nothing

lookupMutantId id = do
  id' <- lookupMutantId_maybe id
  case id' of
    Just mId -> return mId
    otherwise -> error $ "Got not-an-id for mutant '" ++
                         (showSDoc $ ppr id) ++ "'"


nameString = occNameString . nameOccName

lookupMutantTyThing tyThing = do
  let name = mutantName . getName $ tyThing
  (env,hpt,pte,dflags) <- ask
  return $ case lookupTypeEnv env name of
             v@(Just _)-> v
             otherwise -> lookupType dflags hpt pte $ name

lookupType dflags hpt pte name -- this function substantially stolen from 
                                     -- ghc/compiler/main/HscTypes.hs
  | nameHasModule name &&  
    not (isOneShot (ghcMode dflags)) &&
    modulePackageId mod == this_pkg 
  = do hm <- lookupUFM hpt (moduleName mod) -- Maybe monad
       lookupNameEnvString (md_types (hm_details hm)) name
  | otherwise
  = lookupNameEnvString pte name
  where mod = nameModule name
        this_pkg = thisPackage dflags

lookupNameEnvString pte n = listToMaybe . nameEnvElts . filterNameEnv match $ pte
  where match thing = (nameString $ getName thing) == (nameString n) &&
                      (occNameSpace $ nameOccName $ getName thing) == 
                        (occNameSpace $ nameOccName n)

nameHasModule  = isJust . nameModule_maybe

mutantCoreModule mod dm = do
  coreMod <- mutantModGuts mod (dm_core_module dm)
  return dm { dm_core_module = coreMod }

mutantModGuts mod mg = do
  tyEnv <- mutantTypeEnv (mg_types mg)
  coreBinds <- withTypeLookups tyEnv $ mutantCoreBinds (mg_binds mg)
  return mg { mg_binds = coreBinds
            , mg_types = tyEnv
            , mg_dir_imps  = mutantDeps (mg_dir_imps mg) mod
            , mg_exports = mutantAvailInfos (mg_exports mg)
            }

mutantTypeEnv env = do
  rec { result <- withTypeLookups result $ do
          newElts <- mapM mutantTyThing $ typeEnvElts env
          return $ extendTypeEnvList env newElts
      }
  return result

mutantDeps imps mod = extendModuleEnv imps mod [(inctimeName, False, noSrcSpan)]

mutantAvailInfos = concatMap mutantAvailInfo
mutantAvailInfo i@(Avail name) = [i, Avail (mutantName name)]
-- does this one need to include our wacky additional data cons?
mutantAvailInfo i@(AvailTC name names) = [i, AvailTC (mutantName name)
                                                     (map mutantName names)]


mutantTyThing (AnId id) = mutantId id >>= return . AnId
mutantTyThing (ADataCon con) = mutantDataCon con >>= return . ADataCon
mutantTyThing (ATyCon con) = mutantTyCon con >>= return . ATyCon
mutantTyThing (AClass cls) = return $ AClass $ mutantClass cls


mutantCoreBinds = (fmap concat . mapM mutantCoreBind)

mutantCoreBind corebind@(NonRec name exp) = do
  newBind <- liftM2 NonRec (mutantCoreBndr name) (mutantExp exp)
  return [corebind, newBind]
mutantCoreBind corebinds@(Rec name_exps) = do
  newNameExps <- map2M mutantCoreBndr mutantExp name_exps
  return [Rec $ name_exps ++ newNameExps]

mutantCoreBndr = mutantId

mutantId :: Var -> TypeLookupM Var
mutantId var = liftM (mk var') (mutantType $ varType var)
  where var' | List.isPrefixOf "$f" (occNameString $ nameOccName $ varName var)
             = mutantClsName (mutantName $ varName var) $ varType var
             | otherwise                                    
             = (mutantName $ varName var)
        mk n t | isTcTyVar var  = mkTcTyVar n t (tcTyVarDetails var)
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


mutantNameIntoSpace :: Name -> NameSpace -> String -> Name
mutantNameIntoSpace oldName nameSpace suffix = sameSortOfName oldName occName mod
  where oldNameString
          -- anonymous vars differ in the unique, but not the name.
          | n == "ds"              = "ds" ++ (show $ getUnique oldName)
          | n == "+"               = "plus"
          | n == "."               = "compose"
          | n == "()"              = "unit"
          | List.isPrefixOf "$c" n = n ++ (show $ getUnique oldName)
          | otherwise              = n
          where n = occNameString $ nameOccName $ oldName
        nameString | oldNameString == "[]" = "BuiltinList_" ++ suffix
                   | oldNameString == ":"  = "BuiltinList_" ++ suffix
                   | otherwise             = oldNameString ++ "_" ++ suffix
        occName = mkOccName nameSpace nameString

sameSortOfName oldName occName mod
  | isInternalName oldName = mkInternalName unique occName (nameSrcSpan oldName)
  | isExternalName oldName = mkExternalName unique (adaptModule $ 
                                                      nameModule oldName)
                                            occName (nameSrcSpan oldName)
  | isWiredInName oldName  = mkSystemName unique occName 
  | otherwise              = mkSystemName unique occName 
  where 
    unique = getUnique occName
    adaptModule mod | modulePackageId mod == primPackageId = inctime
                    | modulePackageId mod == rtsPackageId  = inctime
                    | modulePackageId mod == basePackageId = inctime
                    | otherwise                            = mod



mutantClsName name instanceType
  = sameSortOfName name (mkOccName (occNameSpace $ nameOccName name) newName) (nameModule name)
  where 
    newName = "$f" ++ clsName' ++ concat paramNames'
    (_, cls) = splitFunTys instanceType
    params = tyConAppArgs cls
    clsName' = (drop 2 $ nameString $ typeName cls) ++ "_incrementalised"
    paramNames = map (nameString . typeName) params
    paramNames' = interlace paramNames (map (++"_incrementalised") paramNames)
    typeName t = getName $ tyConAppTyCon t

-- in The Real World, everything lives in a monad that lets you pluck new
-- uniques out of the unique supply. I didn't want to do that, so we
-- append a string according to the purpose that we want the unique for,
-- make the name with that string, and just use the unique from that new
-- name. Essentially we piggy-back on FastString's impure trickery.
mutantNameUnique oldName nameSpace suffix
  = setNameUnique oldName (nameUnique mutantName)
  where mutantName = mutantNameIntoSpace oldName nameSpace suffix

mutantName oldName = mutantNameIntoSpace oldName
                                         (occNameSpace $ nameOccName oldName)
                                         "incrementalised"
mutantExp :: Expr CoreBndr -> TypeLookupM (Expr CoreBndr)
mutantExp (Var id) = liftM Var $ lookupOrMutantId id
mutantExp (App expr arg)
  | isTypeArg arg = do expr' <- mutantExp expr
                       arg' <- mutantExp arg
                       return $ App (App expr' arg) arg'
  | otherwise     = liftM2 App (mutantExp expr) (mutantExp arg)
-- for \ a -> b, need to check if a is a (incrementalize_type a)_hoist. 
-- If so, produce a (incrementalize_type b)_identity.
mutantExp (Lam id expr) = do 
  expr' <- mutantExp expr
  id' <- lookupOrMutantId id
  let iType = varType id'
  let oType = exprType expr'

  let def = (DEFAULT, [], expr')
  let hoist = (DataAlt $ lookupDataConByAdd iType AddConHoist
              ,[]
              ,dataConAtType (lookupDataConByAdd oType AddConIdentity)
                             oType
              )
  let result | isTyVar id = Lam id $ Lam id' expr'
             | otherwise  = Lam id'
                                (Case (Var $ id')
                                      id'
                                      oType
                                      [def, hoist])

  return result

mutantExp (Let bind expr) = liftM2 (foldl (\e b -> Let b e))
                                    (mutantExp expr)
                                    (mutantCoreBind bind)
mutantExp c@(Case expr id type_ alts) = liftM4 Case 
                                                (mutantExp expr)
                                                (mutantCoreBndr id)
                                                (mutantType type_)
                                                (liftM concat $ sequence
                                                 [mutantAlts alts,
                                                  -- use the list monad return
                                                  fmap return (replaceAlt' c),
                                                  builderAlts' c])
mutantExp (Cast expr coercion) = liftM2 Cast
                                         (mutantExp expr)
                                         (mutantType coercion)
mutantExp (Type type_) = liftM Type (mutantType type_)
mutantExp (Note note expr) = liftM (Note note) (mutantExp expr)

mutantAlts = liftM catMaybes . mapM mutantAlt

mutantAlt :: Alt CoreBndr -> TypeLookupM (Maybe (Alt CoreBndr))
mutantAlt (DataAlt _, [], _) = return Nothing -- these cases get handled by replaceAlt
mutantAlt ((DataAlt dataCon), binds, expr) = do
  dataCon' <- lookupMutantDataCon dataCon
  binds' <- mapM mutantId binds
  expr' <- mutantExp expr
  return $ Just $ ((DataAlt dataCon'), binds', expr')
mutantAlt (DEFAULT, [], expr) = do
  expr' <- mutantExp expr
  return $ Just (DEFAULT, [], expr')
mutantAlt _ = return Nothing -- if we handle changes-moving-into-a-value, then we should
                              -- probably do something for literals here
                              --
replaceAlt' c@(Case expr id type_ alts) = do
  destType <- mutantType type_
  srcType  <- mutantType $ exprType expr
  let destCon = lookupDataConByAdd destType AddConReplacement
  let srcCon  = lookupDataConByAdd srcType  AddConReplacement
  return (DataAlt $ srcCon
         ,[exprVar expr]
         ,App (dataConAtType destCon destType) c)


builderAlts' (Case expr id type_ alts) = (liftM concat . mapM builderAlt') alts

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
-- hoist value, and elsewhere arrange for length_incrementalised to respond to
-- a hoist value by making the term go away. More work is required to make the
-- right thing happen when more than one hoist value is involved.
--
-- When we incrementalise the RHS, it will expect incrementalised values to
-- exist for all the other terms in the expression (x, in this case, although
-- it is not used in the expression). In order to make those terms available,
-- we construct replace values for each argument provided as part of the
-- incrementalised_build constructor and assign them to the incrementalised
-- name of the argument.
builderAlt' a@(DataAlt dataCon, binds, expr) = mapM (builderAltAtIndex' a) builders
  where builders = builderMutantDataConIndexes dataCon

builderAltAtIndex' (DataAlt dataCon, vars, expr) builderConIndex = do
  type_' <- mutantType type_
  let builderCon = lookupDataConByBuilderIndex type_'
                                               builderConIndex
  replaceVarId <- mutantId replaceVar
  replaceVarValue <- hoistValue replaceVar
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



dataConValue addConType var = do
  mType <- mutantType $ varType var
  let con = lookupDataConByAdd mType addConType
  return $ dataConAtType con mType

hoistValue = dataConValue AddConHoist
--replaceValue = fmap App (dataConValue AddConReplacement) . return
replaceValue var = do
  con <- dataConValue AddConReplacement var
  return (App con (Var var))

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
  return $ mkForAllTy tyVar $ mkForAllTy tyVar' ty'

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
   --mkFunTyCon (mutantName $ getName tyCon) (duplicateKindArgs $ tyConKind tyCon)
 
  let newTyCon | isAlgTyCon tyCon      = mutantAlgTyCon
               | isAbstractTyCon tyCon = makeTyConAbstract $ mutantAlgTyCon
               | isClassTyCon tyCon    = mutantClassTyCon
               | isFunTyCon tyCon      = mutantFunTyCon
               | isPrimTyCon tyCon     = tyCon
               | otherwise = trace ("Don't know how to mutate tyCon " ++ 
                                     (showSDoc.ppr.getName$tyCon) ++
                                      " :: " ++ (showSDoc.ppr$tyCon))
                                   tyCon
  return newTyCon
  where
    name = mutantName $ getName tyCon
    kind = duplicateKindArgs $ tyConKind tyCon
    isRec = boolToRecFlag $ isRecursiveTyCon tyCon
    predTys = tyConStupidTheta tyCon -- can we incrementalise constraints?
    parent = tyConParent tyCon
    hasGen = tyConHasGenerics tyCon
    declaredGadt = isGadtSyntaxTyCon tyCon


-- the idea here is that a kind of * -> * should become a kind of * -> * -> *, to make room
-- for the incrementalised versions of the type parameters.
duplicateKindArgs (splitTyConApp_maybe -> Just (tyCon, []))
  = mkTyConApp tyCon [] 
-- if we go from TyConApp to TyConApp, then we get a (-> * * *) rather than a (* -> * -> *).
-- Going from TyConApp to FunTy doesn't seem to have that problem.
duplicateKindArgs (splitTyConApp_maybe -> Just (tyCon, kinds))
  = foldl (flip mkFunTy) (head argKinds) $ (tail argKinds) ++ argKinds ++ [resultKind]
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

data AdditionalConType = AddConReplacement
                       | AddConHoist
                       | AddConIdentity
  deriving Show
additionalConTypes = [AddConReplacement, AddConHoist, AddConIdentity]
additionalConSuffix AddConReplacement = "replace"
additionalConSuffix AddConHoist       = "hoist"
additionalConSuffix AddConIdentity    = "identity"

builderMutantDataCons oldTyCon newTyCon dataCon
  = map (builderMutantDataCon oldTyCon newTyCon dataCon) indexes
  where indexes = builderMutantDataConIndexes dataCon

builderMutantDataConIndexes dataCon = map fst index_types
  where all_index_types = zip [0..] (dataConOrigArgTys dataCon)
        index_types = filter (\(i, t) -> tyTyConMatches t tyCon)
                             all_index_types
        tyCon = dataConTyCon dataCon

tyTyConMatches (splitTyConApp_maybe -> Just (tyCon1, _)) tyCon2
  = tyCon1 == tyCon2
tyTyConMatches _ _ = False

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

builderConSuffix n = "build_using_" ++ show n
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

mutantClass = id

exprVar (Var id) = id
exprVar other = error ("exprVar " ++ (showSDoc $ ppr other))

exprType :: Expr CoreBndr -> Type
exprType (Var id) = varType id
exprType (Lit lit) = literalType lit
exprType (App expr arg) = snd $ splitFunTys (exprType expr)
exprType (Lam id expr) = mkFunTy (varType id) (exprType expr)
exprType (Let bind expr) = exprType expr
exprType (Case expr id type_ alts) = type_
exprType (Cast expr coercion) = coercion
exprType (Type type_) = type_
exprType (Note note expr) = exprType expr

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
                                (Var $ dataConWorkId con)
                                typeArgs
  where typeArgs = case splitTyConApp_maybe type_ of
                     Just (_, args) -> args
                     otherwise      -> []

inctimeName = mkModuleName "Inctime"
inctime = mkModule mainPackageId inctimeName

process targetFile moduleName = do
  defaultErrorHandler defaultDynFlags $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      let dflags_xopts = foldl xopt_set dflags
                                    [ Opt_Cpp
                                    , Opt_ImplicitPrelude
                                    , Opt_MagicHash]
      let dflags_dopts = foldl dopt_set dflags_xopts
                                    [Opt_EmitExternalCore
                                    , Opt_ForceRecomp]
                                    --, Opt_D_verbose_core2core]
      let dflags' = dflags_dopts -- { verbosity = 4 }
      liftIO $ addWay WayDebug
      setSessionDynFlags dflags'
      target <- guessTarget targetFile Nothing
      target_runtime <- guessTarget "Inctime" Nothing
      setTargets [target]
      liftIO $ putStrLn "loading."
      load LoadAllTargets
      liftIO $ putStrLn "loaded. Getting modSum."
      modGraph <- getModuleGraph
      liftIO $ putStrLn $ showSDoc $ ppr modGraph
      modSum <- getModSummary $ mkModuleName moduleName
      liftIO $ putStrLn "Got modSum. Parsing."
      p <- parseModule modSum
      t <- typecheckModule p
      d <- desugarModule t
      d' <- mutantCoreModule (ms_mod modSum) d
      liftIO $ do
        putStrLn $ showSDoc $ ppr $ mg_binds $ dm_core_module d'
        putStrLn $ ms_hspp_file modSum

      liftIO $ do
        lintPrintAndFail d'

      setSessionDynFlags $ dflags' { hscOutName = targetFile ++ ".s"
                                   , extCoreName = targetFile ++ ".hcr"
                                   , outputFile = Just $ targetFile ++ ".o"
                                   }
      (hscGenOutput hscBatchCompiler) (dm_core_module d') modSum Nothing
      return ()

lintPrintAndFail desugaredModule = do 
  let bindings = mg_binds $ dm_core_module desugaredModule
  let (errors, warnings) = lintCoreBindings bindings
  when ((not $ isEmptyBag errors) || (not $ isEmptyBag warnings)) $ do
    putStrLn $ showSDoc $ pprMessageBag errors
    putStrLn $ showSDoc $ pprMessageBag warnings
    exitFailure

main = do
  args <- getArgs
  when (length args > 2) $ do
    putStrLn $ "Usage: Incrementalizer from [moduleName]"
    exitFailure
  let (from, moduleName) = case args of
                             [from] -> (from, from)
                             [from, moduleName] -> (from, moduleName)
  process from moduleName

showDataConDetails dataCon = (concat $ List.intersperse "\n" [
   ("NAME " ++ (showSDoc $ ppr $ dataConName dataCon))
  ,("  isInfix " ++ (showSDoc $ ppr $ dataConIsInfix dataCon))
  ,("  strictMarks " ++ (showSDoc $ ppr $ dataConStrictMarks dataCon))
  ,("  fieldLabels " ++ (showSDoc $ ppr $ dataConFieldLabels dataCon))
  ,("  univTyVars " ++ (showSDoc $ ppr $ dataConUnivTyVars dataCon))
  ,("  exTyVars " ++ (showSDoc $ ppr $ dataConExTyVars dataCon))
  ,("  eqSpec " ++ (showSDoc $ ppr $ dataConEqSpec dataCon))
  ,("  dictTheta " ++ (showSDoc $ ppr $ dataConDictTheta dataCon)) -- dataConTheta on GHC 7.4?
  ,("  origArgs " ++ (showSDoc $ ppr $ dataConOrigArgTys dataCon))
  ,("  origRes " ++ (showSDoc $ ppr $ dataConOrigResTy dataCon))
  ,("  tyCon " ++ (showSDoc $ ppr $ dataConTyCon dataCon))
  ,("  repType " ++ (showSDoc $ ppr $ dataConRepType dataCon))
  ,("  stupidTheta " ++ (showSDoc $ ppr $ dataConStupidTheta dataCon))])
