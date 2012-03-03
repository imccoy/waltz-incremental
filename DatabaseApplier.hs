import Language.Core.Core
import qualified Language.Haskell.Syntax as Hs
import qualified Language.Haskell.Pretty as HsPretty

import Incrementalizer
import Utils

typeclass_instances (Module (M (_, _, name)) tdefs vdefgs) = Hs.HsModule hs_nowhere (Hs.Module $ name ++ "instances") Nothing imports $ typeclass_instances_tdefs tdefs
  where imports = [import_decl name, import_decl "Radtime", import_decl "DbRadtime"]
        import_decl name = Hs.HsImportDecl { Hs.importLoc = hs_nowhere
                                           , Hs.importModule = Hs.Module name
                                           , Hs.importQualified = False
                                           , Hs.importAs = Nothing
                                           , Hs.importSpecs = Nothing
                                           }


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
        pat = [input_change_pat, Hs.HsPVar $ Hs.HsIdent "address"]
        exp = Hs.HsDo $ map Hs.HsQualifier (invocations ++ [Hs.HsApp (Hs.HsVar $ Hs.UnQual $ Hs.HsIdent "return") Hs.unit_con])
        invocations = zipWith invocation input_change_pat_arg_names [0..]
        invocation change_n n = Hs.HsParen $ Hs.HsApp 
                                 (Hs.HsApp 
                                   (Hs.HsVar $ Hs.UnQual $ Hs.HsIdent "applyDbInputChange")
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
  writeFileContents "Bprime.dbinstances.hs" $ "{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}\n" ++ (HsPretty.prettyPrint typeclass_instances_code)
  return ()
