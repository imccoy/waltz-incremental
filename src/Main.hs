{-# LANGUAGE ViewPatterns, DoRec, TupleSections #-}

module Main where

import Control.Monad
import Control.Monad.Reader hiding (liftIO)
import qualified Data.List as List
import Data.Maybe
import Debug.Trace
import Safe
import System.Environment (getArgs)
import System.Exit


import Bag
import BasicTypes 
import GHC.Paths ( libdir )
import CoreLint
import CoreMonad
import CoreSyn
import DataCon
import DynFlags
import ErrUtils
import HscTypes hiding (lookupDataCon, lookupType)
import HscMain
import IdInfo
import Literal
import MkId
import Module
import Name hiding (varName)
import NameEnv hiding (lookupNameEnv)
import OccName hiding (varName)
import qualified OccName as OccName
import Outputable
import StaticFlags
import TyCon
import Type
import TysWiredIn
import Unique
import UniqFM
import Var

import GHC hiding (exprType)


interlace :: [a] -> [a] -> [a]
interlace [] [] = []
interlace (a:as) (b:bs) = a:b:(interlace as bs)

interlace3 as bs cs = interlaceN [as, bs, cs]

interlaceN [] = []
interlaceN ([]:_) = []
interlaceN ((a:as):xs) = a:(interlaceN $ xs ++ [as])

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

lookupMutantTyThing tyThing = do
  let name = mutantName . getName $ tyThing
  (env,hpt,pte,dflags) <- ask
  return $ case lookupTypeEnv env name of
             v@(Just _)-> v
             otherwise -> lookupType dflags hpt pte (nameModule_maybe name)
                                                    (lookupNameEnv name)

lookupInctimeTyThing n ns = do
  (env,hpt,pte,dflags) <- ask
  return $ lookupType dflags hpt pte (Just $ mkModule (thisPackage dflags)
                                                      (mkModuleName "Inctime")) 
                                     (lookupNameEnvString n ns)


lookupPreludeTyThing :: String -> 
                        String -> 
                        NameSpace -> 
                        TypeLookupM (Maybe TyThing)
lookupPreludeTyThing m n ns = do
  (env,hpt,pte,dflags) <- ask
  return $ lookupType dflags hpt pte (Just $ mkModule basePackageId
                                                      (mkModuleName m)) 
                                     (lookupNameEnvString n ns)



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

lookupTypeM :: (Maybe Module) -> 
               (NameEnv TyThing -> Maybe TyThing) -> 
               TypeLookupM (Maybe TyThing)
lookupTypeM maybeMod lookup = do
  (env,hpt,pte,dflags) <- ask
  return $ case lookup env of
             v@(Just _) -> v
             otherwise -> lookupType dflags hpt pte maybeMod lookup

lookupType :: DynFlags 
           -> HomePackageTable
           -> PackageTypeEnv
           -> Maybe Module
           -> (PackageTypeEnv  -> Maybe TyThing)
           -> Maybe TyThing
lookupType dflags hpt pte maybeMod lookup -- this function substantially stolen from 
                                      -- ghc/compiler/main/HscTypes.hs
  | isJust maybeMod &&  
    not (isOneShot (ghcMode dflags)) &&
    modulePackageId mod == this_pkg 
  = do hm <- lookupUFM hpt (moduleName mod) -- Maybe monad
       lookup (md_types (hm_details hm))
  | otherwise
  = lookup pte
  where mod = fromJustNote "lookupType module" maybeMod
        this_pkg = thisPackage dflags

lookupNameEnv :: Name -> TypeEnv -> Maybe TyThing
lookupNameEnv n pte = lookupNameEnvString (nameString n)
                                          (occNameSpace . nameOccName $ n)
                                          pte

lookupNameEnvString :: String -> NameSpace -> TypeEnv -> Maybe TyThing
lookupNameEnvString n space pte
  = listToMaybe . nameEnvElts . filterNameEnv match $ pte
  where match thing = (nameString $ getName thing) == n &&
                      (occNameSpace $ nameOccName $ getName thing) == space

nameHasModule  = isJust . nameModule_maybe

mutantCoreModule mod dm = do
  coreMod <- mutantModGuts mod (dm_core_module dm)
  return dm { dm_core_module = coreMod }

mutantModGuts mod mg = do
  (tyEnv, typeclassBinds) <- mutantTypeEnv (mg_types mg)
  coreBinds <- withTypeLookups tyEnv $ mutantCoreBinds (mg_binds mg)
  return mg { mg_binds = coreBinds ++ typeclassBinds
            , mg_types = tyEnv
            , mg_dir_imps  = mutantDeps (mg_dir_imps mg) mod
            , mg_exports = mutantAvailInfos (mg_exports mg)
            }

mutantTypeEnv env = do
  rec { (result, classBinds) <- withTypeLookups result $ do
          elt_classBinds <- mapM mutantTyThing $ typeEnvElts env
          let classBinds = concatMap snd elt_classBinds
          let newElts = map fst elt_classBinds ++
                        map AnId (bindersOfBinds classBinds)
          return (extendTypeEnvList env newElts, classBinds)
      }
  return (result, classBinds)

mutantDeps imps mod = extendModuleEnv imps mod [(inctimeName, False, noSrcSpan)]

mutantAvailInfos = concatMap mutantAvailInfo
mutantAvailInfo i@(Avail name) = [i, Avail (mutantName name)]
-- does this one need to include our wacky additional data cons?
mutantAvailInfo i@(AvailTC name names) = [i, AvailTC (mutantName name)
                                                     (map mutantName names)]


mutantTyThing :: TyThing -> TypeLookupM (TyThing, [CoreBind])
mutantTyThing (AnId id) = mutantId id >>= return . (,[]) . AnId
mutantTyThing (ADataCon con) = mutantDataCon con >>= return . (,[]) . ADataCon
mutantTyThing (ATyCon con) = mutantTyCon con >>= return . (\(c,bs) -> (ATyCon c, bs))
mutantTyThing (AClass cls) = return $ (AClass $ mutantClass cls, [])

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


mutantNameIntoSpace :: Name -> NameSpace -> String -> Name
mutantNameIntoSpace oldName nameSpace suffix
  = sameSortOfName oldName occName mod
  where 
    nameString = (adaptName oldName) ++ "_" ++ suffix
    occName = mkOccName nameSpace nameString

adaptName name = adaptName' $ occNameString $ nameOccName name
  where adaptName' "ds" = "ds" ++ (show $ getUnique name)
        adaptName' "+"  = "plus"
        adaptName' "."  = "compose"
        adaptName' "()" = "unit"
        adaptName' "[]" = "BuiltinList"
        adaptName' ":" = "BuiltinList"
        adaptName' s 
          | List.isPrefixOf "$c" s = s ++ (show $ getUnique name)
          | otherwise              = s

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
expIncrementalisedDictionary (Type t@(splitTyConApp_maybe -> Just (tyCon, args))) = do
  inst <- incrementalisedDictionaryInstance t
  argsMutants <- mapM mutantType args
  argsInsts <- mapM expIncrementalisedDictionary (map Type args)
  return $ mkApps (Var $ inst) $ interlace3 (map Type args)
                                            (map Type argsMutants)
                                            argsInsts
expIncrementalisedDictionary (Type t) 
  = liftM Var $ incrementalisedDictionaryInstance t

incrementalisedDictionaryType :: Type -> Type -> TypeLookupM Type
incrementalisedDictionaryType baseType incrementalisedType = do
  inc <- liftM (mkTyConTy . tyThingTyCon . fromJustNote
                 ("The type of the Incrementalised "++
                  "typeclass dictionary"))
               (lookupInctimeTyThing "T:Incrementalised"
                                     OccName.tcName)
  return $ mkAppTys inc [baseType, incrementalisedType]

lookupAndConvertInctimeTyThing converter msg n space
  = liftM (converter . fromJustNote msg)
          (lookupInctimeTyThing n
                                space)
incrementalisedTest n
  = lookupAndConvertInctimeTyThing tyThingId 
                                   ("incrementalised" ++ n ++ "Test")
                                   ("isIncrementalised" ++ n)
                                   OccName.varName
incrementalisedReplaceTest :: TypeLookupM Var
incrementalisedReplaceTest = incrementalisedTest "Replace"
incrementalisedHoistTest :: TypeLookupM Var
incrementalisedHoistTest = incrementalisedTest "Hoist"
incrementalisedIdentityMk :: TypeLookupM Var
incrementalisedIdentityMk
  = lookupAndConvertInctimeTyThing tyThingId
                                   "incrementalisedIdentityMk"
                                   "mkIncrementalisedIdentity"
                                   OccName.varName
incrementalisedReplaceMk :: TypeLookupM Var
incrementalisedReplaceMk
  = lookupAndConvertInctimeTyThing tyThingId
                                   "incrementalisedReplaceMk"
                                   "mkIncrementalisedReplace"
                                   OccName.varName


incrementalisedReplaceExtractor :: TypeLookupM Var
incrementalisedReplaceExtractor
  = lookupAndConvertInctimeTyThing tyThingId
                                   "incrementalisedReplaceExtractor"
                                   "extractReplaceValue"
                                   OccName.varName

objContainer :: TypeLookupM TyCon
objContainer
  = lookupAndConvertInctimeTyThing tyThingTyCon
                                   "objContainer"
                                   "Obj"
                                   OccName.tcName

incrementalisedDictionaryInstance :: Type -> TypeLookupM Var
incrementalisedDictionaryInstance type_
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
    (_, cls) = splitFunTys instanceType
    (clsTy, paramsTys) = maybe (error $ "mutantClsOccName couldn't split " ++
                                        (showSDoc $ ppr cls))
                               id
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
    (_, cls) = splitFunTys instanceType
    (clsTy, _) = maybe (error $"incrementalisedDictionaryInstanceName"++
                                       " couldn't split " ++
                                       (showSDoc $ ppr cls))
                               id
                               $ splitTyConApp_maybe cls
    baseName = getName $ clsTy


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
                       argD <- expIncrementalisedDictionary arg
                       return $ App (App (App expr' arg) arg') argD
                       --return $ App (App expr' arg) arg'
  | otherwise     = liftM2 App (mutantExp expr) (mutantExp arg)
-- for \ a -> b, need to check if a is a (incrementalize_type a)_hoist. 
-- If so, produce a (incrementalize_type b)_identity.
mutantExp (Lam id expr) = do 
  expr' <- mutantExp expr
  id' <- lookupOrMutantId id
  idD <- varIncrementalisedDictionary id (typeFor id) (typeFor id')
  let iType = varType id'
  let oType = exprType expr'

  test <- liftM2 mkApps (liftM Var incrementalisedHoistTest)
                        (return $ [Type (varType id)
                                  , Type iType
                                  , Var idD
                                  , Var id'])
  idDvalue <- expIncrementalisedDictionary $ Type $ varType id
  let lets = [NonRec idD idDvalue]
                

  let def = (DEFAULT, [], expr')

  hoist <- do
    mk <- incrementalisedIdentityMk
    dict <- expIncrementalisedDictionary $ Type $ exprType expr
    return (DataAlt trueDataCon
              ,[]
              ,mkApps (Var mk)
                      [Type (exprType expr)
                      , Type oType
                      , dict]
              )
  let result | isTyVar id = Lam id $ Lam id' $ Lam idD expr'
             | otherwise  = Lam id' $ mkLets lets $
                                (Case test
                                      (testArgVar (nameString $ varName id')
                                                  (mkTyConTy boolTyCon)
                                                  0)
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
                                                (introduceSpecialAltCases c =<<
                                                 mutantAlts alts)
                                                  -- use the list monad return
                                                  -- fmap return (replaceAlt' c),
                                                  --builderAlts' c])
mutantExp (Cast expr coercion) = liftM2 Cast
                                         (mutantExp expr)
                                         (mutantType coercion)
mutantExp (Type type_) = liftM Type (mutantType type_)
mutantExp (Note note expr) = liftM (Note note) (mutantExp expr)

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
    return $ Let (NonRec (exprVar expr) (App replaceExtractor expr')) $
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
builderAlt' a@(DataAlt dataCon, binds, expr) = mapM (builderAltAtIndex' a)
                                                    builders
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
  return $ dataConValueByType addConType mType
dataConValueByType addConType mType = 
  let con = lookupDataConByAdd mType addConType
   in dataConAtType con mType

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
  let mkTestAlts alts
        = Lam (testVar 0) $
            Case (Var $ testVar 0)
                 (testVar 0)
                 (mkTyConTy boolTyCon)
                 ((DEFAULT, [], Var falseDataConId):alts)
  let mkTest addConType argTypes
        = mkTestAlts $ [(DataAlt $ lookupDataConByAdd (mkTyConTy mutantTyCon)
                                                      addConType
                       , testArgVars argTypes
                       , Var trueDataConId)]
  let isReplace = mkTest AddConReplacement [baseType] 
  let isHoist = mkTest AddConHoist []
  let buildAlts
        = concat (map (\dataCon -> 
                         map (\idx -> 
                                let builderCon = lookupDataConByBuilderIndex
                                                         (mkTyConTy mutantTyCon)
                                                         (idx)
                                 in (DataAlt builderCon
                                    ,testArgVars (dataConOrigArgTys builderCon)
                                    ,Var trueDataConId))
                             (builderMutantDataConIndexes dataCon))
                 (tyConDataCons tyCon))
  let isBuild = mkTestAlts buildAlts
  let isIdentity = mkTest AddConIdentity []

  let mkBuilder addConType argTypes
        = mkLams (testArgVars argTypes)
                 (mkApps (dataConValueByType addConType (mkTyConTy mutantTyCon))
                         (map (Type . mkTyVarTy) (tyConTyVars mutantTyCon) ++
                          map Var (testArgVars argTypes)))

  let mkReplace = mkBuilder AddConReplacement [baseType]
  let mkIdentity = mkBuilder AddConIdentity []

  callUndefined <- liftM (Var . tyThingId . fromJustNote "callUndefined")
                         (lookupPreludeTyThing ""
                                               "undefined"
                                               OccName.varName)

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

  extractBuild <- do
    objContainerTy <- objContainer
    return $ Lam (testVar 0) $
               Lam (testArgVar baseName (mkTyConTy intTyCon) 1) $
                 App callUndefined
                     (Type $ mkTyConTy objContainerTy)

  return $ NonRec classInstanceId $ 
                mkLams (tyConTyVars mutantTyCon)
                       (mkApps classDataConExp [ isReplace
                                               , isBuild
                                               , isHoist
                                               , isIdentity
                                               , mkReplace
                                               , mkIdentity
                                               , extractReplace
                                               , extractBuild])
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


testArgVar baseName type_ n = mkLocalVar VanillaId 
                                (mkInternalName (getUnique $ tvn) 
                                                tvn
                                                noSrcSpan)
                                type_
                                vanillaIdInfo
  where tvn = testVarOccName n baseName
testVarOccName n baseName = mkOccName OccName.varName
                                      (baseName ++ "_test" ++ show n)



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

typeFor :: Var -> Type
typeFor v
 | isTyVar v = mkTyVarTy v
 | otherwise = varType v

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
        showAllModuleContents $ mg_types $ dm_core_module $ d'
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

showAllModulesContents :: Ghc ()
showAllModulesContents = do
  getSession >>= return . eltsUFM . hsc_HPT >>= mapM (showAllModuleContents . 
                                                      md_types . 
                                                      hm_details)
  getSession >>= 
    liftIO . hscEPS >>=
    return . eps_PTE >>= 
    showAllModuleContents

showAllModuleContents mod = do
  liftIO $ putStrLn $ showSDoc $ ppr $ eltsUFM mod

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
