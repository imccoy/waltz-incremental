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
mutant_alt ty (Acon qDcon tbinds [] exp) = [Acon (adjust_type_reference "replace" ty) (mutant_tbinds tbinds) [("a", Tvar "a")] (App (Dcon $ adjust_type_reference "replace" ty) exp)]
mutant_alt ty (Acon qDcon tbinds vbinds exp) = (builds [] vbinds 0) [Acon (incrementalise_name qDcon) (mutant_tbinds tbinds) (mutant_vbinds vbinds) (mutant_exp exp)]
  where builds :: [Vbind] -> [Vbind] -> Int -> [Alt] -> [Alt]
        builds vbinds1 (vbind@(vbind_var,vbind_ty):vbinds2) n cons
          | ty == vbind_ty = let con = apply_to_name (++ "_build_using_" ++ (show n)) $ incrementalise_name qDcon
                                 name_to_replace = vbind_var :: String
                                 replacement_exp = Var $ hoistable_type_reference vbind_ty
                                 new_exp = replace_exp replacement_exp (snd $ incrementalise_name $ (Nothing, name_to_replace)) (mutant_exp exp)
                                 acon = Acon con (mutant_tbinds tbinds) (mutant_vbinds $ vbinds1 ++ vbinds2) new_exp
                              in builds (vbinds1 ++ [vbind]) vbinds2 (n+1) (acon:cons)
          | otherwise      = builds (vbinds1 ++ [vbind]) vbinds2 (n+1) cons
        builds _ _ _ cons = cons
mutant_alt ty (Alit lit exp) = [Alit lit $ mutant_exp exp]
mutant_alt ty (Adefault exp) = [Adefault $ mutant_exp exp]

hoistable_type_reference = adjust_type_reference "hoist"


adjust_type_reference n (Tcon tcon) = apply_to_name (++ "_" ++ n) $ incrementalise_name tcon
adjust_type_reference n (Tapp ty1 ty2) = adjust_type_reference n ty1

adjust_type_reference n other = error $ show $ Data.toConstr other


mutant_exp (Var qVar) = Var $ incrementalise_name qVar
mutant_exp (Dcon qDcon) = Dcon $ incrementalise_name qDcon
mutant_exp (Lit lit) = Lit lit
mutant_exp (App exp1 exp2) = App (mutant_exp exp1) (mutant_exp exp2)
mutant_exp (Appt exp ty) = Appt (mutant_exp exp) (mutant_ty ty)
mutant_exp (Lam (Tb tbind) exp) = Lam (mutant_bind $ Tb tbind) (mutant_exp exp)

-- for \ a -> b, need to check if a is a (incrementalize_type a)_hoist. If so, produce a (incrementalize_type b)_hoist
mutant_exp (Lam (Vb vbind) exp) = case type_of_exp exp of
                                    Right param_exp_type -> Lam (Vb newBind) $ Case (Var $ unqual $ fst newBind) vbind (snd newBind) [
                                                                                 Acon (hoistable_type_reference $ snd vbind) [] [] (Var $ hoistable_type_reference $ param_exp_type),
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


coreFileContents = do
  file <- openFile "B.hcr" ReadMode
  contents <- hGetContents file
  case parse contents 0 of
    (FailP e) -> do putStrLn "HORRIBLY WRONG"
                    putStrLn e
                    exitFailure
    (OkP e) -> return e

writeFileContents core =  do
  file <- openFile "Bprime.hcr" WriteMode
  hPutStr file $ show core
  hClose file

main = do
  core <- coreFileContents
  let mutant_core = mutant core
  putStrLn $ show $ mutant_core
  writeFileContents mutant_core
  return ()
