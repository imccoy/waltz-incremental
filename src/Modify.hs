{-# LANGUAGE ViewPatterns, TupleSections, ScopedTypeVariables, DoRec #-}

module Modify (ModifyFuncs (..), modifyCoreModule) where

import Control.Monad
import Safe

import Class
import CoreSyn
import DataCon
import GHC
import HscTypes hiding (lookupDataCon, lookupType)
import IfaceSyn (IfaceInst (..))
import IfaceType (toIfaceTyCon_name)
import InstEnv (extendInstEnvList,
                is_cls, is_dfun, is_tcs, is_flag)
import Module
import Var



import Lookups
import Names

data ModifyFuncs = ModifyFuncs {
   modifyCoreBinds :: [CoreBind] -> TypeLookupM [CoreBind],
   modifyName :: Name -> Name,
   modifyLookupTyCon :: TyCon -> TypeLookupM TyCon,
   modifyId :: Var -> TypeLookupM Var,
   modifyTyCon :: TyCon -> TypeLookupM (TyCon, [CoreBind], [Instance]),
   modifyClass :: Class -> Class,
   modifyDataCon :: DataCon -> TypeLookupM DataCon
}


modifyCoreModule mf mod dm = do
  coreMod <- modifyModGuts mf mod (dm_core_module dm)
  return $ dm { dm_core_module = coreMod }

modifyModGuts mf mod mg = do
  (tyEnv, typeclassBinds, instances) <- modifyTypeEnv mf (mg_types mg)
  coreBinds <- withTypeLookups tyEnv $ modifyCoreBinds mf (mg_binds mg)
  let newIfaceInsts = map (\inst -> IfaceInst {
                              ifDFun = varName $ is_dfun inst
                             ,ifOFlag = is_flag inst
                             ,ifInstCls = is_cls inst
                             ,ifInstTys = map (fmap toIfaceTyCon_name)
                                              (is_tcs inst)
                             ,ifInstOrph = Nothing -- should be safe, since all
                                                   -- instances we generate are
                                                   -- for types defined in this
                                                   -- module
                            }
                          )
                          instances
  mg_exports' <- (withTypeLookups tyEnv
                      $ modifyAvailInfos mf (mg_exports mg)) >>=
                   (return . (++) (concatMap
                                      (\n -> [Avail n, AvailTC n [n]]) $
                                           map (varName . fst) $
                                             flattenBinds typeclassBinds))
  return $ mg { mg_binds = coreBinds ++ typeclassBinds
               , mg_types = tyEnv
               , mg_dir_imps  = modifyDeps mf (mg_dir_imps mg) mod
               , mg_exports = mg_exports'
               , mg_inst_env = extendInstEnvList (mg_inst_env mg) instances
               , mg_insts = instances
               }

modifyTypeEnv mf env = do
  rec { (result, classBinds, instances) <- withTypeLookups result $ do
          elt_classBinds_instances <- mapM (modifyTyThing mf) $ typeEnvElts env
          let elts       = map       (\(a,_,_) -> a) elt_classBinds_instances
          let classBinds = concatMap (\(_,b,_) -> b) elt_classBinds_instances
          let instances  = concatMap (\(_,_,c) -> c) elt_classBinds_instances
          let newElts = elts ++ map AnId (bindersOfBinds classBinds)
          return (extendTypeEnvList env newElts, classBinds, instances)
      }
  return (result, classBinds, instances)

modifyDeps mf imps mod = extendModuleEnv imps
                                         mod
                                         [(inctimeName, False, noSrcSpan)]

modifyAvailInfos mf = liftM concat . mapM (modifyAvailInfo mf)
modifyAvailInfo mf i@(Avail name) = return [i, Avail (modifyName mf name)]
modifyAvailInfo mf i@(AvailTC name names) = do
   tycon <- lookupTyThingName name >>= return . tyThingTyCon . (fromJustNote $
                                           "no modify tycon for exports" ++
                                               nameString name)
   tycon' <- modifyLookupTyCon mf tycon
   let cons = map dataConName $ tyConDataCons tycon'
   return [i, AvailTC (modifyName mf name)
                      (cons ++ (map (modifyName mf) names))]


modifyTyThing :: ModifyFuncs ->
                 TyThing ->
                 TypeLookupM (TyThing, [CoreBind], [Instance])
modifyTyThing mf (AnId id) = modifyId mf id >>= return . (,[],[]) . AnId
modifyTyThing mf (ADataCon con) = modifyDataCon mf con 
                                 >>= return . (,[],[]) . ADataCon
modifyTyThing mf (ATyCon con) = modifyTyCon mf con
                                 >>= return . (\(c,bs,is) -> (ATyCon c, bs, is))
modifyTyThing mf (AClass cls) = return $ (AClass $ modifyClass mf cls, [],[])


