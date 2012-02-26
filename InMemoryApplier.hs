import Language.Core.Core
import qualified Language.Haskell.Syntax as Hs
import qualified Language.Haskell.Pretty as HsPretty

import Incrementalizer
import Utils

typeclass_instances (Module (M (_, _, name)) tdefs vdefgs) = Hs.HsModule hs_nowhere (Hs.Module $ name ++ "instances") Nothing imports $ typeclass_instances_tdefs tdefs
  where imports = [Hs.HsImportDecl { Hs.importLoc = hs_nowhere
                                   , Hs.importModule = Hs.Module name
                                   , Hs.importQualified = False
                                   , Hs.importAs = Nothing
                                   , Hs.importSpecs = Nothing
                                   },
                   Hs.HsImportDecl { Hs.importLoc = hs_nowhere
                                   , Hs.importModule = Hs.Module "Radtime"
                                   , Hs.importQualified = False
                                   , Hs.importAs = Nothing
                                   , Hs.importSpecs = Nothing
                                   }
                   ]

typeclass_instances_tdefs = map typeclass_instances_tdef
typeclass_instances_tdef (Data qTcon tbinds cdefs) = Hs.HsInstDecl hs_nowhere context (Hs.UnQual$ Hs.HsIdent "Incrementalised") types [decl]
  where 
        incrementalise_type_con = Hs.HsTyVar $ Hs.HsIdent $ snd $ incrementalise_name qTcon
        base_type_con = Hs.HsTyVar $ Hs.HsIdent $ snd $ qTcon
        base_type = foldl Hs.HsTyApp base_type_con (map (\(tvar, kind) -> Hs.HsTyVar $ Hs.HsIdent tvar) tbinds)
        incrementalise_type = foldl Hs.HsTyApp incrementalise_type_con (map (\(tvar, kind) -> Hs.HsTyVar $ Hs.HsIdent tvar) $ tbinds ++ (mutant_tbinds tbinds))
        context = zipWith (\(tvar_base, kind_base) (tvar, kind) -> (Hs.UnQual $ Hs.HsIdent "Incrementalised", [Hs.HsTyVar $ Hs.HsIdent tvar, Hs.HsTyVar $ Hs.HsIdent tvar_base])) tbinds (mutant_tbinds tbinds)
        types = [incrementalise_type, base_type]
        decl = Hs.HsFunBind matches
        matches = (map (applyInputChangeCdef qTcon) cdefs) ++ (concat $ map (applyInputChangeBuild qTcon) cdefs)

applyInputChangeCdef qTcon (Constr qDcon tbinds tys) = Hs.HsMatch hs_nowhere (Hs.HsIdent "applyInputChange") pat (Hs.HsUnGuardedRhs exp) []
  where (input_change_pat, input_change_pat_arg_names) = hs_pat_names (incrementalise_name qDcon) "_change" (length tys)
        (base_pat, base_pat_arg_names) = hs_pat_names qDcon "_base" (length tys)
        pat = [input_change_pat, base_pat]
        exp = hs_con_exp qDcon exp_args
        exp_args = zipWith (\change_n base_n -> Hs.HsParen $ Hs.HsApp 
                               (Hs.HsApp 
                                 (Hs.HsVar $ Hs.UnQual $ Hs.HsIdent "applyInputChange")
                                 (Hs.HsVar $ Hs.UnQual change_n))
                               (Hs.HsVar $ Hs.UnQual base_n)
                           ) input_change_pat_arg_names base_pat_arg_names
applyInputChangeBuild qTcon (Constr qDcon tbinds tys) = generate_cdef_builds qTcon tys builder
  where builder tys1 ty tys2 n = [Hs.HsMatch hs_nowhere (Hs.HsIdent "applyInputChange") pat (Hs.HsUnGuardedRhs exp) []]
          where (input_change_pat, input_change_pat_arg_names) = hs_pat_names (apply_to_name (++ "_build_using_" ++ (show n)) $ incrementalise_name qDcon) "" (length tys1 + length tys2)
                pat = [input_change_pat, Hs.HsPVar $ Hs.HsIdent "base"]
                exp = hs_con_exp qDcon exp_args
                exp_args = map (Hs.HsVar . Hs.UnQual) arg_names
                arg_names = (arg_names_before ++ (Hs.HsIdent "base"):arg_names_after)
                (arg_names_before, arg_names_after) = splitAt (length tys1) input_change_pat_arg_names

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
  writeFileContents "Bprime.instances.hs" $ "{-# LANGUAGE MultiParamTypeClasses #-}\n" ++ (HsPretty.prettyPrint typeclass_instances_code)
  return ()
