import Control.Monad
import Control.Monad.Error
import qualified Data.Data as Data
import Data.Either
import Data.Maybe
import Debug.Trace
import IO
import Language.Core.Core
import Language.Core.Parser
import Language.Core.ParseGlue
import System.Exit
import Zcode

import qualified Language.Haskell.Syntax as Hs
import qualified Language.Haskell.Pretty as HsPretty

mutant (Module name tdefs vdefgs) = Module name tdefs' vdefgs'
  where tdefs' = tdefs ++ (mutant_tdefs tdefs)
        vdefgs' = vdefgs ++ (mutant_vdefgs vdefgs)

mutant_tdefs = map mutant_tdef
mutant_tdef (Data qTcon tbinds cdefs) = Data (incrementalise_name qTcon) (tbinds ++ mutant_tbinds tbinds) $ (mutant_cdefs cdefs) ++ (mutant_cdefs_builds qTcon tbinds cdefs) ++ [hoist_cdef qTcon]

mutant_cdefs_builds qTcon tbinds cdefs = concat $ map (mutant_cdef_builds qTcon) cdefs
mutant_cdef_builds qTcon (Constr qDcon tbinds tys) = builds [] tys 0
  where builds tys1 (ty:tys2) n 
          | ty_matches ty = [Constr (apply_to_name (++ "_build_using_" ++ (show n)) $ incrementalise_name qDcon) tbinds (tys1 ++ tys2)] ++ (builds (tys1 ++ [ty]) tys2 (n+1))
          | otherwise     = builds (tys1 ++ [ty]) tys2 (n+1)
        builds _ _ _ = []
        ty_matches (Tcon t)      = t == qTcon
        ty_matches (Tapp bind t) = ty_matches bind
        ty_matches t             = False

mutant_tbinds = map mutant_tbind
mutant_tbind (tvar, kind) = (incrementalise_string tvar, kind)

mutant_cdefs = map mutant_cdef
mutant_cdef (Constr dcon tbinds tys) = Constr (incrementalise_name dcon) (mutant_tbinds tbinds) (mutant_tys tys)

hoist_cdef qTcon = Constr (hoistable_type_reference $ Tcon qTcon) [] []

mutant_tys = map mutant_ty
mutant_ty (Tvar tvar) = Tvar $ incrementalise_string tvar
mutant_ty (Tcon qTcon) = Tcon $ incrementalise_name qTcon
mutant_ty (Tapp ty1 ty2) = Tapp (Tapp (mutant_ty ty1) ty2) (mutant_ty ty2)
mutant_ty (Tforall tbind ty) = Tforall tbind (mutant_ty ty)
mutant_ty (TransCoercion ty1 ty2) = TransCoercion (mutant_ty ty1) (mutant_ty ty2)
mutant_ty (SymCoercion ty) = SymCoercion (mutant_ty ty)
mutant_ty (InstCoercion ty1 ty2) = InstCoercion (mutant_ty ty1) (mutant_ty ty2)
mutant_ty (LeftCoercion ty) = LeftCoercion (mutant_ty ty)
mutant_ty (RightCoercion ty) = RightCoercion (mutant_ty ty)

mutant_vdefgs = map mutant_vdefg
mutant_vdefg (Rec vdefs) = Rec $ map mutant_vdef vdefs
mutant_vdefg (Nonrec vdef) = Nonrec $ mutant_vdef vdef

mutant_vdef (Vdef (qVar, ty, exp)) = Vdef (incrementalise_name qVar, mutant_ty ty, mutant_exp exp)

mutant_vbinds = map mutant_vbind
mutant_vbind (var, ty) = (zencode $ incrementalise_string var, mutant_ty ty)
mutant_bind (Vb vbind) = Vb $ mutant_vbind vbind
mutant_bind (Tb tbind) = Tb $ mutant_tbind tbind

mutant_alts ty = concat . (map $ mutant_alt ty)
--mutant_alt ty (Acon qDcon tbinds [] exp) = [Acon (adjust_type_reference "replace" ty) (mutant_tbinds tbinds) [("a", Tvar "a")] (App (Dcon $ adjust_type_reference "replace" ty) exp)]
mutant_alt ty (Acon qDcon tbinds [] exp) = []
mutant_alt ty (Acon qDcon tbinds vbinds exp) = (builds [] vbinds 0) [Acon (incrementalise_name qDcon) (mutant_tbinds tbinds) (mutant_vbinds vbinds) (mutant_exp exp)]
  where builds :: [Vbind] -> [Vbind] -> Int -> [Alt] -> [Alt]
        builds vbinds1 (vbind@(vbind_var,vbind_ty):vbinds2) n cons
          | ty == vbind_ty = let con = apply_to_name (++ "_build_using_" ++ (show n)) $ incrementalise_name qDcon
                                 acon = Acon con (mutant_tbinds tbinds) (vbinds1 ++ vbinds2) $ 
                                          Let hoist_defn $ 
                                            foldr (\replacement_vdefg exp -> Let replacement_vdefg exp) (mutant_exp exp) replacement_vdefgs
                                 name_to_hoist = vbind_var :: String
                                 names_to_replace = vbinds1 ++ vbinds2
                                 hoist_defn = Nonrec $ Vdef (incrementalise_name (Nothing, name_to_hoist), Tvar $ snd $ hoistable_type_reference vbind_ty, Var $ hoistable_type_reference vbind_ty)
                                 replacement_vdefgs = map replacement_vdefg names_to_replace
                                   where replacement_vdefg :: Vbind -> Vdefg
                                         replacement_vdefg (var, ty) = Nonrec $ Vdef (incrementalise_name (Nothing, var), mutant_ty $ snd vbind, 
                                                                                        App (Var $ replacement_type_reference ty) (Var (Nothing, var)) )
                
                                 -- new_exp = replace_exp replacement_exp (snd $ incrementalise_name $ (Nothing, name_to_replace)) (mutant_exp exp)
                                 -- replacement_exp = Var $ hoistable_type_reference vbind_ty
                              in builds (vbinds1 ++ [vbind]) vbinds2 (n+1) (acon:cons)
          | otherwise      = builds (vbinds1 ++ [vbind]) vbinds2 (n+1) cons
        builds _ _ _ cons = cons
mutant_alt ty (Alit lit exp) = [Alit lit $ mutant_exp exp]
mutant_alt ty (Adefault exp) = [Adefault $ mutant_exp exp]

hoistable_type_reference = adjust_type_reference "hoist"
identity_type_reference = adjust_type_reference "identity"
replacement_type_reference = adjust_type_reference "replace"


adjust_type_reference n (Tcon tcon) = apply_to_name (++ "_" ++ n) $ incrementalise_name tcon
adjust_type_reference n (Tapp ty1 ty2) = adjust_type_reference n ty1

adjust_type_reference n other = error $ show $ Data.toConstr other


mutant_exp (Var qVar) = Var $ incrementalise_name qVar
mutant_exp (Dcon qDcon) = Dcon $ incrementalise_name qDcon
mutant_exp (Lit lit) = Lit lit
mutant_exp (App exp1 exp2) = App (mutant_exp exp1) (mutant_exp exp2)
mutant_exp (Appt exp ty) = Appt (mutant_exp exp) (mutant_ty ty)
mutant_exp (Lam (Tb tbind) exp) = Lam (mutant_bind $ Tb tbind) (mutant_exp exp)

-- for \ a -> b, need to check if a is a (incrementalize_type a)_hoist. If so, produce a (incrementalize_type b)_identity.
mutant_exp (Lam (Vb vbind) exp) = case type_of_exp exp of
                                    Right param_exp_type -> Lam (Vb newBind) $ Case (Var $ unqual $ fst newBind) vbind (snd newBind) [
                                                                                 Acon (hoistable_type_reference $ snd vbind) [] [] (Var $ identity_type_reference $ param_exp_type),
                                                                                 Adefault (mutant_exp exp) ]
                                                                               where (Vb newBind) = mutant_bind $ Vb vbind
                                    Left e -> Lam (mutant_bind $ Vb vbind) (mutant_exp exp)
mutant_exp (Let vdefg exp) = Let (mutant_vdefg vdefg) (mutant_exp exp)
mutant_exp (Case exp vbind ty alts) = Case (mutant_exp exp) (mutant_vbind vbind) (mutant_ty ty) (mutant_alts (snd vbind) alts)
mutant_exp (Cast exp ty) = Cast (mutant_exp exp) (mutant_ty ty)
mutant_exp (Note string exp) = Note string $ mutant_exp exp
mutant_exp (External string ty) = error "mutant_exp don't know externals from infernos"

replace_exp :: Exp -> Var -> Exp -> Exp
replace_exp replacement name_to_replace var@(Var qVar)
  | name_to_replace == (snd qVar) = replacement
  | otherwise                     = var
replace_exp replacement name_to_replace (App exp1 exp2) = App (replace_exp replacement name_to_replace exp1) (replace_exp replacement name_to_replace exp2)
replace_exp replacement name_to_replace (Appt exp ty) = Appt (replace_exp replacement name_to_replace exp) ty
replace_exp replacement name_to_replace (Lam bind exp) = Lam bind (replace_exp replacement name_to_replace exp)
replace_exp replacement name_to_replace (Let vdefg exp) = Let vdefg (replace_exp replacement name_to_replace exp)
replace_exp replacement name_to_replace (Case exp vbind ty alts) = Case (replace_exp replacement name_to_replace exp) vbind ty alts
replace_exp replacement name_to_replace (Cast exp ty) = Cast (replace_exp replacement name_to_replace exp) ty
replace_exp replacement name_to_replace (Note string exp) = Note string $ replace_exp replacement name_to_replace exp
replace_exp _ _ exp = exp

type_of_exp (Lam (Vb vbind) exp)     = type_of_exp exp
type_of_exp (Lam (Tb tbind) exp)     = Right $ Tvar $ fst tbind
type_of_exp (Let vdefg exp)          = type_of_exp exp
type_of_exp (Case exp vbind ty alts) = Right $ ty
type_of_exp (Appt exp ty)            = Right $ ty
type_of_exp (App exp _)              = type_of_exp exp
type_of_exp (Cast exp ty)            = Right $ ty
type_of_exp (Note string exp)        = type_of_exp exp
type_of_exp exp                      = Left $ "Unknown type of exp" ++ (show $ Data.toConstr exp )  ++ " " ++ show exp





incrementalise_string s = s ++ "_incrementalised"
incrementalise_name = apply_to_name (zencode . incrementalise_string)

apply_to_name f (mod, name) = (mod, f name)

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
        exp = foldl Hs.HsApp (Hs.HsCon $ Hs.UnQual $ Hs.HsIdent $ snd qDcon) exp_args
        exp_args = zipWith (\change_n base_n -> Hs.HsParen $ Hs.HsApp 
                               (Hs.HsApp 
                                 (Hs.HsVar $ Hs.UnQual $ Hs.HsIdent "applyInputChange")
                                 (Hs.HsVar $ Hs.UnQual change_n))
                               (Hs.HsVar $ Hs.UnQual base_n)
                           ) input_change_pat_arg_names base_pat_arg_names
applyInputChangeBuild qTcon (Constr qDcon tbinds tys) = builds [] tys 0
  where builds tys1 (ty:tys2) n 
          | ty_matches ty = [Hs.HsMatch hs_nowhere (Hs.HsIdent "applyInputChange") pat (Hs.HsUnGuardedRhs exp) []] ++ (builds (tys1 ++ [ty]) tys2 (n+1))
          | otherwise     = builds (tys1 ++ [ty]) tys2 (n+1)
                             where (input_change_pat, input_change_pat_arg_names) = hs_pat_names (apply_to_name (++ "_build_using_" ++ (show n)) $ incrementalise_name qDcon) "" (length tys1 + length tys2)
                                   pat = [input_change_pat, Hs.HsPVar $ Hs.HsIdent "base"]
                                   exp = let (before, after) = splitAt (length tys1) input_change_pat_arg_names
                                          in foldl Hs.HsApp (Hs.HsVar $ Hs.UnQual $ Hs.HsIdent $ snd qDcon)$ map (Hs.HsVar . Hs.UnQual) (before ++ (Hs.HsIdent "base"):after)
        builds _ _ _ = []
        ty_matches (Tcon t)      = t == qTcon
        ty_matches (Tapp bind t) = ty_matches bind
        ty_matches t             = False

hs_pat_names qDcon suffix n = (Hs.HsPApp hs_con args, names)
  where hs_con = Hs.UnQual $ Hs.HsIdent $ snd qDcon
        args = map Hs.HsPVar names
        names = hs_n_pat_names suffix n

hs_n_pat_names suffix n = take n $ map (\n -> Hs.HsIdent (n:suffix)) ['a'..]

hs_nowhere = Hs.SrcLoc "nowhere" 0 0

coreFileContents = do
  file <- openFile "B.hcr" ReadMode
  contents <- hGetContents file
  case parse contents 0 of
    (FailP e) -> do putStrLn "HORRIBLY WRONG"
                    putStrLn e
                    exitFailure
    (OkP e) -> return e

writeFileContents filename content =  do
  file <- openFile filename WriteMode
  hPutStr file $ content
  hClose file

main = do
  core <- coreFileContents
  let mutant_core = mutant core
  putStrLn $ show $ mutant_core
  writeFileContents "Bprime.hcr" $ show mutant_core
  let typeclass_instances_code = typeclass_instances core
  putStrLn $ show typeclass_instances_code
  writeFileContents "Bprime.instances.hs" $ "{-# LANGUAGE MultiParamTypeClasses #-}\n" ++ (HsPretty.prettyPrint typeclass_instances_code)
  return ()
