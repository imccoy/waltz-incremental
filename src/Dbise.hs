{-# LANGUAGE ViewPatterns, TupleSections, ScopedTypeVariables, DoRec #-}
module Dbise (dbiseCoreModule) where

import Modify

dbiseCoreModule = modifyCoreModule $ ModifyFuncs {
  modifyCoreBinds = return,
  modifyName = id,
  --lookupmodifyTyCon :: TyCon -> TypeLookupM TyCon
  --lookupmodifyTyCon tyCon
  --   = lookupTyThingName (tyConName tyCon) >>=
  --                         return . tyThingTyCon . fromJustNote "lookupmodifyTyCon"
  modifyLookupTyCon = return,
  modifyId = return,
  modifyDataCon = return,
  modifyTyCon = return . (,[],[]),
  modifyClass = id
}
