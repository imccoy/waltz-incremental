module DatabaseApplier (db_applier) where

import qualified Data.List as List

import Language.Core.Core
import qualified Language.Haskell.Syntax as Hs

import Incrementalizer
import Utils

db_applier = typeclass_instances

typeclass_instances (Module (M (_, _, name)) tdefs vdefgs) = add_imports (Hs.HsModule hs_nowhere (Hs.Module $ name ++ "instances") Nothing [] $ (typeclass_instances_tdefs tdefs) ++ (db_strategy tdefs)) [name, "Radtime", "DbRadtime"]

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

typeclass_instances_tdefs tdefs = concat $ [db_incrementalised_instances_tdefs tdefs, db_initialised_instances_tdefs tdefs]

db_incrementalised_instances_tdefs = map db_incrementalised_instances_tdef
db_incrementalised_instances_tdef tdef@(Data qTcon tbinds cdefs) = typeclass_instance "DbIncrementalised" (mutant_tbinds tbinds) tdef incrementalise_name (tbinds ++ (mutant_tbinds tbinds)) applyDbInputChangeCdef

db_initialised_instances_tdefs = map db_initialised_instances_tdef
db_initialised_instances_tdef tdef@(Data qTcon tbinds cdefs) = typeclass_instance "DbInitialise" tbinds tdef id tbinds (setInitialValueCdef qTcon)

typeclass_instance name deps (Data qTcon tbinds cdefs) type_name_transformation all_tbinds method_builder = Hs.HsInstDecl hs_nowhere context (Hs.UnQual$ Hs.HsIdent name) [type_] [decl]
  where 
        type_con = (Hs.HsTyVar $ Hs.HsIdent $ snd $ type_name_transformation qTcon)
        type_ = foldl Hs.HsTyApp type_con (map (\(tvar, kind) -> Hs.HsTyVar $ Hs.HsIdent tvar) all_tbinds)
        context = map (\(tvar, kind) -> (Hs.UnQual $ Hs.HsIdent name, [Hs.HsTyVar $ Hs.HsIdent tvar])) deps
        decl = Hs.HsFunBind $ (map method_builder cdefs) -- ++ (concat $ map (applyDbInputChangeBuild qTcon) cdefs)



applyDbInputChangeCdef (Constr qDcon tbinds tys) = Hs.HsMatch hs_nowhere (Hs.HsIdent "applyDbInputChange") pat (Hs.HsUnGuardedRhs exp) []
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


setInitialValueCdef qTcon (Constr qDcon tbinds tys) = Hs.HsMatch hs_nowhere (Hs.HsIdent "setInitialValue") pat (Hs.HsUnGuardedRhs exp) []
  where (base_pat, base_pat_arg_names) = hs_pat_names (qDcon) "_base" (length tys)
        pat = [Hs.HsPVar $ Hs.HsIdent "handle", Hs.HsPVar $ Hs.HsIdent "structure", base_pat]
        exp = Hs.HsApp (Hs.HsApp (Hs.HsApp (app_with_handle_and_structure "setInitialValue'")
                                           (Hs.HsLit $ Hs.HsString $ snd qTcon))
                                 (Hs.HsLit $ Hs.HsString $ snd qDcon))
                       invocations
        invocations = Hs.HsList $ map (invocation) base_pat_arg_names
        invocation name = Hs.HsApp (app_with_handle_and_structure "setInitialValue") (Hs.HsVar $ Hs.UnQual $ name)
        app_with_handle_and_structure f = Hs.HsApp (Hs.HsApp (Hs.HsVar $ Hs.UnQual $ Hs.HsIdent f)
                                                             (Hs.HsVar $ Hs.UnQual $ Hs.HsIdent "handle"))
                                                   (Hs.HsVar $ Hs.UnQual $ Hs.HsIdent "structure")


hs_con_exp qDcon exp_args = foldl Hs.HsApp exp_con exp_args
  where exp_con = (Hs.HsCon $ Hs.UnQual $ Hs.HsIdent $ snd qDcon)

hs_pat_names qDcon suffix n = (Hs.HsPApp hs_con args, names)
  where hs_con = Hs.UnQual $ Hs.HsIdent $ snd qDcon
        args = map Hs.HsPVar names
        names = hs_n_pat_names suffix n

hs_n_pat_names suffix n = take n $ map (\n -> Hs.HsIdent (n:suffix)) ['a'..]

