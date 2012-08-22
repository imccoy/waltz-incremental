module Lookups where

import Control.Monad
import Control.Monad.Reader hiding (liftIO)
import Data.Maybe
import Safe

import Class (Class)
import CoreMonad
import DataCon (DataCon)
import DynFlags
import HscTypes hiding (lookupDataCon, lookupType)
import Module
import Name hiding (varName)
import NameEnv hiding (lookupNameEnv)
import OccName hiding (varName)
import qualified OccName as OccName
import Outputable
import TyCon
import Type
import UniqFM
import Var

import Names (mutantName)




instance MonadFix Ghc where
  -- stolen from ghc source
  mfix f = Ghc $ \s -> mfix (\x -> unGhc (f x) s)

type TypeLookupM a = Reader ( TypeEnv
                            , HomePackageTable
                            , PackageTypeEnv
                            , DynFlags)
                            a

withTypeLookupsDefault :: TypeLookupM a -> Ghc a
withTypeLookupsDefault f = do
  withTypeLookups emptyTypeEnv f

withTypeLookups :: TypeEnv -> TypeLookupM a -> Ghc a
withTypeLookups typeEnv f = do
  session <- getSession
  eps <- liftIO $ hscEPS session -- ExternalPackageState
  let hpt = hsc_HPT session
  let dynflags = hsc_dflags session
  return $ runReader f (typeEnv, hpt, eps_PTE eps, dynflags)

lookupMutantTyThing tyThing = do
  let name = mutantName . getName $ tyThing
  lookupTyThingName name

lookupTyThingName name = do
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
  | isPrimTyCon tyCon                    = return tyCon
  | otherwise                            = do
    tyThing <- lookupMutantTyThing (ATyCon tyCon)
    case tyThing of
      Just (ATyCon tyCon) -> return tyCon
      otherwise           -> error $ "Got not-a-tycon for mutant '" ++ 
                                     (showSDoc $ ppr tyCon) ++ "'" ++
                                     (showSDoc $ ppr $ modulePackageId $ nameModule $ getName $ tyCon)

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

lookupTypeClassString :: String -> TypeLookupM Class
lookupTypeClassString clsNameS = fmap (tyThingClass . fromJustNote 
                                         ("lookupTypeClassString " ++ clsNameS)) $
                                 lookupInctimeTyThing clsNameS OccName.tcName

incrementalisedDictionaryType :: Type -> Type -> TypeLookupM Type
incrementalisedDictionaryType baseType incrementalisedType = do
  inc <- liftM (mkTyConTy . tyThingTyCon . fromJustNote
                 ("The type of the Incrementalised "++
                  "typeclass dictionary"))
               (lookupInctimeTyThing "T:Incrementalised"
                                     OccName.tcName)
  return $ mkAppTys inc [baseType, incrementalisedType]



applicableDictionaryType :: Type -> Type -> TypeLookupM Type
applicableDictionaryType baseType incrementalisedType = do
  app <- liftM (mkTyConTy . tyThingTyCon . fromJustNote
                 ("The type of the ApplicableIncrementalised "++
                  "typeclass dictionary"))
               (lookupInctimeTyThing "T:ApplicableIncrementalised"
                                     OccName.tcName)

  return $ mkAppTys app [baseType, incrementalisedType]



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
incrementalisedIdentityTest :: TypeLookupM Var
incrementalisedIdentityTest = incrementalisedTest "Identity"
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

allIdentityOrReplaceFn :: TypeLookupM Var
allIdentityOrReplaceFn
  = lookupAndConvertInctimeTyThing tyThingId
                                   "allIdentityOrReplaceFn"
                                   "allIdentityOrReplace"
                                   OccName.varName

incrementalisedThingTyCon :: TypeLookupM TyCon
incrementalisedThingTyCon
  = lookupAndConvertInctimeTyThing tyThingTyCon
                                   "incrementalisedThingTyCon"
                                   "IncrementalisedThing"
                                   OccName.tcName

incrementalisedThingTy = incrementalisedThingTyCon >>= return . mkTyConTy

incrementalisedThingDataCon :: TypeLookupM DataCon
incrementalisedThingDataCon
  = lookupAndConvertInctimeTyThing tyThingDataCon
                                   "incrementalisedThingDataCon"
                                   "IncrementalisedThing"
                                   OccName.dataName

incrementalisedReplaceExtractor :: TypeLookupM Var
incrementalisedReplaceExtractor
  = lookupAndConvertInctimeTyThing tyThingId
                                   "incrementalisedReplaceExtractor"
                                   "extractReplaceValue"
                                   OccName.varName
inputChangeApplier :: TypeLookupM Var
inputChangeApplier
  = lookupAndConvertInctimeTyThing tyThingId
                                   "inputChangeApplier"
                                   "applyInputChange"
                                   OccName.varName

incBoxIncCon :: TypeLookupM DataCon
incBoxIncCon
  = lookupAndConvertInctimeTyThing tyThingDataCon
                                   "incBoxIncCon"
                                   "IncBox_incrementalised"
                                   OccName.dataName
incBoxTyCon :: TypeLookupM TyCon
incBoxTyCon
  = lookupAndConvertInctimeTyThing tyThingTyCon
                                   "incBoxTyCon"
                                   "IncBox"
                                   OccName.tcName

noIncLamId :: TypeLookupM Id
noIncLamId
  = lookupAndConvertInctimeTyThing tyThingId
                                   "noIncLamId"
                                   "noIncLam"
                                   OccName.varName

noIncAppId :: TypeLookupM Id
noIncAppId
  = lookupAndConvertInctimeTyThing tyThingId
                                   "noIncAppId"
                                   "noIncApp"
                                   OccName.varName



lookupPreludeFn :: String -> String -> TypeLookupM Var
lookupPreludeFn m n = do
  liftM (tyThingId . fromJustNote ("lookupPreludeFn " ++ m ++ "." ++ n))
        (lookupPreludeTyThing m n OccName.varName)


