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
mutant_tdef (Data qTcon tbinds cdefs) = Data (incrementalise_name qTcon) (tbinds ++ mutant_tbinds tbinds) $ (mutant_cdefs cdefs) ++ (mutant_cdefs_builds qTcon tbinds cdefs)

mutant_cdefs_builds qTcon tbinds cdefs = concat $ map (mutant_cdef_builds qTcon tbinds) cdefs
mutant_cdef_builds qTcon dataTbinds (Constr qDcon tbinds tys) = builds [] tys 0
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
-- mutant_alt ty (Acon qDcon tbinds vbinds exp) = map mutant_alt_possibility $ mutant_ty_cons ty qDcon
--   where mutant_alt_possibility possibility = [Acon (incrementalise_name qDcon) (mutant_tbinds tbinds) (mutant_vbinds vbinds) (mutant_exp exp)]
mutant_alt ty (Acon qDcon tbinds vbinds exp) = [Acon (incrementalise_name qDcon) (mutant_tbinds tbinds) (mutant_vbinds vbinds) (mutant_exp exp)]
mutant_alt ty (Alit lit exp) = [Alit lit $ mutant_exp exp]
mutant_alt ty (Adefault exp) = [Adefault $ mutant_exp exp]

mutant_exp (Var qVar) = Var $ incrementalise_name qVar
mutant_exp (Dcon qDcon) = Dcon $ incrementalise_name qDcon
mutant_exp (Lit lit) = Lit lit
mutant_exp (App exp1 exp2) = App (mutant_exp exp1) (mutant_exp exp2)
mutant_exp (Appt exp ty) = Appt (mutant_exp exp) (mutant_ty ty)
mutant_exp (Lam bind exp) = Lam (mutant_bind bind) (mutant_exp exp)
mutant_exp (Let vdefg exp) = Let (mutant_vdefg vdefg) (mutant_exp exp)
mutant_exp (Case exp vbind ty alts) = Case (mutant_exp exp) (mutant_vbind vbind) (mutant_ty ty) (mutant_alts ty alts)
mutant_exp (Cast exp ty) = Cast (mutant_exp exp) (mutant_ty ty)
mutant_exp (Note string exp) = Note string $ mutant_exp exp
mutant_exp (External string ty) = error "mutant_exp don't know externals from infernos"

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
