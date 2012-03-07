import qualified Data.List as List

import Language.Core.Core
import qualified Language.Haskell.Syntax as Hs
import qualified Language.Haskell.Pretty as HsPretty

import Incrementalizer
import Utils

derivations (Module _ tdefs _) = concat $ List.intersperse "\n" $ concat $ map derivation_tdef tdefs
derivation_tdef (Data qTcon tbinds _) = [instance_code "Show", typeable_instance_code, instance_code "Data"]
  where instance_code clazz = "deriving instance (" ++ tvar_instances clazz ++ ") => " ++ clazz ++ " (" ++ (snd qTcon) ++ " " ++ (concat $ List.intersperse " " tvars) ++ ")"
        typeable_instance_code =  "deriving instance " ++ typeable_name tbinds++ " (" ++ (snd qTcon) ++ ")"
        typeable_name [] = "Typeable"
        typeable_name tbinds = "Typeable" ++ (show $ length tbinds)
        tvars = zipWith (\a b -> [b]) tbinds ['a'..]
        tvar_instances clazz = concat $ List.intersperse " " $ map (\a -> clazz ++ " " ++ a) tvars

typeclass_instances (Module (M (_, _, name)) tdefs vdefgs) = Hs.HsModule hs_nowhere (Hs.Module $ name ++ "instances") Nothing imports $ (typeclass_instances_tdefs tdefs) ++ (db_strategy tdefs)
  where imports = [import_decl name, import_decl "Radtime", import_decl "DbRadtime", import_decl "Data.Data"]
        import_decl name = Hs.HsImportDecl { Hs.importLoc = hs_nowhere
                                           , Hs.importModule = Hs.Module name
                                           , Hs.importQualified = False
                                           , Hs.importAs = Nothing
                                           , Hs.importSpecs = Nothing
                                           }

is_recursive (Data qTcon tbinds cdefs) tdefs = (ty_matches qTcon) `List.any` types_mentioned_in cdefs
  where
    types_mentioned_in cdefs = types_mentioned_in' cdefs []
    types_mentioned_in' cdefs set = foldr types_mentioned_in_cdef set cdefs
    types_mentioned_in_cdef (Constr qDcon tbinds tys) set = types_mentioned_in_cdef' tys set
    types_mentioned_in_cdef' (ty:tys) set
      | ty `List.notElem` set = let set' = types_mentioned_in' (cdefs_for ty) (ty:set)
                                 in types_mentioned_in_cdef' tys set'
      | otherwise             = set
    types_mentioned_in_cdef' [] set = set
    cdefs_for ty = concat $ map (\(Data _ _ cdefs) -> cdefs) $ filter (\(Data qTcon _ _) -> ty_matches qTcon ty) tdefs

db_strategy tdefs = [ Hs.HsTypeSig hs_nowhere [Hs.HsIdent "db_structure"] (Hs.HsQualType [] $ Hs.HsTyVar $ Hs.HsIdent "DbStructure")
                    , Hs.HsFunBind [Hs.HsMatch hs_nowhere (Hs.HsIdent "db_structure") [] (Hs.HsUnGuardedRhs exp) []]]
  where exp = Hs.HsList $ map (db_strategy_tdef tdefs) tdefs
db_strategy_tdef tdefs tdef@(Data qTcon tbinds cdefs) = Hs.HsTuple [name, db_strategy, constructors]
  where name = Hs.HsLit $ Hs.HsString $ snd qTcon
        db_strategy = case is_recursive tdef tdefs of
                True -> Hs.HsVar $ Hs.UnQual $ Hs.HsIdent $ "Separate"
                False -> Hs.HsVar $ Hs.UnQual $ Hs.HsIdent $ "Inline"
        constructors = Hs.HsList $ map constructor cdefs
        constructor (Constr qDcon tbinds tys) = Hs.HsTuple [Hs.HsLit $ Hs.HsString $ snd qDcon, Hs.HsList $ map type_name tys]
        type_name = Hs.HsLit . Hs.HsString . type_name'
        type_name' (Tcon (_, n)) = n
        type_name' (Tapp a b) = type_name' a
        type_name' _ = ""

typeclass_instances_tdefs = map typeclass_instances_tdef
typeclass_instances_tdef (Data qTcon tbinds cdefs) = Hs.HsInstDecl hs_nowhere context (Hs.UnQual$ Hs.HsIdent "DbIncrementalised") types [decl]
  where 
        incrementalise_type_con = Hs.HsTyVar $ Hs.HsIdent $ snd $ incrementalise_name qTcon
        base_type_con = Hs.HsTyVar $ Hs.HsIdent $ snd $ qTcon
        base_type = foldl Hs.HsTyApp base_type_con (map (\(tvar, kind) -> Hs.HsTyVar $ Hs.HsIdent tvar) tbinds)
        incrementalise_type = foldl Hs.HsTyApp incrementalise_type_con (map (\(tvar, kind) -> Hs.HsTyVar $ Hs.HsIdent tvar) $ tbinds ++ (mutant_tbinds tbinds))
        context = map (\(tvar, kind) -> (Hs.UnQual $ Hs.HsIdent "DbIncrementalised", [Hs.HsTyVar $ Hs.HsIdent tvar])) (mutant_tbinds tbinds)
        types = [incrementalise_type]
        decl = Hs.HsFunBind matches
        matches = (map (applyDbInputChangeCdef qTcon) cdefs) -- ++ (concat $ map (applyDbInputChangeBuild qTcon) cdefs)

applyDbInputChangeCdef qTcon (Constr qDcon tbinds tys) = Hs.HsMatch hs_nowhere (Hs.HsIdent "applyDbInputChange") pat (Hs.HsUnGuardedRhs exp) []
  where (input_change_pat, input_change_pat_arg_names) = hs_pat_names (incrementalise_name qDcon) "_change" (length tys)
        pat = [Hs.HsPVar $ Hs.HsIdent "handle", Hs.HsPVar $ Hs.HsIdent "structure", input_change_pat, Hs.HsPVar $ Hs.HsIdent "address"]
        exp = Hs.HsDo $ map Hs.HsQualifier (invocations ++ [Hs.HsApp (Hs.HsVar $ Hs.UnQual $ Hs.HsIdent "return") Hs.unit_con])
        invocations = zipWith invocation input_change_pat_arg_names [0..]
        invocation change_n n = Hs.HsParen $ Hs.HsApp 
                                 (Hs.HsApp
                                   (Hs.HsApp
                                     (Hs.HsApp 
                                       (Hs.HsVar $ Hs.UnQual $ Hs.HsIdent "applyDbInputChange")
                                       (Hs.HsVar $ Hs.UnQual $ Hs.HsIdent "handle"))
                                     (Hs.HsVar $ Hs.UnQual $ Hs.HsIdent "structure"))
                                   (Hs.HsVar $ Hs.UnQual change_n))
                                 (new_address qDcon n)
        new_address constructor position = Hs.HsParen $ Hs.HsApp (Hs.HsApp (Hs.HsApp (Hs.HsVar $ Hs.UnQual $ Hs.HsIdent "appendAddress")
                                                                                     (Hs.HsVar $ Hs.UnQual $ Hs.HsIdent "address")) 
                                                                           (Hs.HsLit $ Hs.HsString $ snd constructor))
                                                                 (Hs.HsLit $ Hs.HsInt $ position)
        make_bind a b = Hs.HsParen (Hs.HsApp (Hs.HsApp (Hs.HsVar $ Hs.UnQual $ Hs.HsIdent "(>>)") a)
                                             b)
{-
 -
 - applyDbInputChangeBuild qTcon (Constr qDcon tbinds tys) = generate_cdef_builds qTcon tys builder
 -  where builder tys1 ty tys2 n = [Hs.HsMatch hs_nowhere (Hs.HsIdent "applyDbInputChange") pat (Hs.HsUnGuardedRhs exp) []]
 -          where (input_change_pat, input_change_pat_arg_names) = hs_pat_names (apply_to_name (++ "_build_using_" ++ (show n)) $ incrementalise_name qDcon) "" (length tys1 + length tys2)
 -                pat = [input_change_pat, Hs.HsPVar $ Hs.HsIdent "base"]
 -                exp = hs_con_exp qDcon exp_args
 -                exp_args = map (Hs.HsVar . Hs.UnQual) arg_names
 -                arg_names = (arg_names_before ++ (Hs.HsIdent "base"):arg_names_after)
 -                (arg_names_before, arg_names_after) = splitAt (length tys1) input_change_pat_arg_names
 -}
hs_con_exp qDcon exp_args = foldl Hs.HsApp exp_con exp_args
  where exp_con = (Hs.HsCon $ Hs.UnQual $ Hs.HsIdent $ snd qDcon)

hs_pat_names qDcon suffix n = (Hs.HsPApp hs_con args, names)
  where hs_con = Hs.UnQual $ Hs.HsIdent $ snd qDcon
        args = map Hs.HsPVar names
        names = hs_n_pat_names suffix n

hs_n_pat_names suffix n = take n $ map (\n -> Hs.HsIdent (n:suffix)) ['a'..]

hs_nowhere = Hs.SrcLoc "nowhere" 0 0


main = do
  core <- coreFileContents
  let typeclass_instances_code = typeclass_instances core
  putStrLn $ show typeclass_instances_code
  writeFileContents "Bprime.dbinstances.hs" $ "{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, DeriveDataTypeable, StandaloneDeriving #-}\n" ++ (HsPretty.prettyPrint typeclass_instances_code) ++ "\n" ++ (derivations core)
  return ()