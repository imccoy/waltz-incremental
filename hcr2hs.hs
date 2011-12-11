import Control.Monad
import Data.Maybe
import Debug.Trace
import IO
import qualified Language.Core.Core as Core
import Language.Core.Parser
import Language.Core.ParseGlue
import qualified Language.Haskell.Syntax as Hs
import Language.Haskell.Pretty
import System.Exit

coreFileContents = do
  file <- openFile "Bprime.hcr" ReadMode
  contents <- hGetContents file
  case parse contents 0 of
    (FailP e) -> do putStrLn "HORRIBLY WRONG"
                    putStrLn e
                    exitFailure
    (OkP e) -> return e

writeFileContents hs =  do
  file <- openFile "Bprime.hs" WriteMode
  hPutStr file $ prettyPrint hs
  hClose file

nowhere = Hs.SrcLoc "nowhere" 0 0

hcr_to_hs (Core.Module (Core.M (Core.P name, ids, id)) tdefs vdefgs) = Hs.HsModule nowhere (Hs.Module id) Nothing [] (transformed_vdefgs vdefgs)

transformed_vdefgs vdefgs = concat $ map transformed_vdefg vdefgs
 where transformed_vdefg (Core.Rec vdefs) = map transformed_vdef vdefs
       transformed_vdefg (Core.Nonrec vdef) = [transformed_vdef vdef]

transformed_vdef (Core.Vdef ((_, name), ty, exp)) = Hs.HsFunBind $ [Hs.HsMatch nowhere (Hs.HsIdent name) [] (Hs.HsUnGuardedRhs $ transformed_exp exp) []]

-- Let Vdefg Exp	 
-- Note String Exp	 
-- External String Ty
transformed_exp (Core.Var ((Just mname), name)) = Hs.HsVar $ Hs.Qual (Hs.Module $ mname_name mname) (Hs.HsIdent name)
transformed_exp (Core.Var (Nothing, name)) = Hs.HsVar $ Hs.UnQual (Hs.HsIdent name)
transformed_exp (Core.Dcon ((Just mname), dcon)) = Hs.HsCon $ Hs.Qual (Hs.Module $ mname_name mname) (Hs.HsIdent dcon)
transformed_exp (Core.Dcon (Nothing, dcon)) = Hs.HsCon $ Hs.UnQual (Hs.HsIdent dcon)
transformed_exp (Core.Lit lit) = Hs.HsLit $ transformed_lit lit
transformed_exp (Core.App exp1 exp2) = Hs.HsApp (transformed_exp exp1) (transformed_exp exp2) 
-- transformed_exp (Core.Appt exp ty) = Hs.HsExpTypeSig nowhere (transformed_exp exp) (transformed_ty ty)
transformed_exp (Core.Lam (Core.Vb (var, _)) exp) = Hs.HsLambda nowhere [Hs.HsPVar $ Hs.HsIdent var] (transformed_exp exp)
transformed_exp (Core.Lam (Core.Tb _) exp) = transformed_exp exp
transformed_exp (Core.Case exp vbind ty alts) = Hs.HsCase (transformed_exp exp) $ map transformed_alt alts
transformed_exp (Core.Cast exp ty) = transformed_exp exp
transformed_exp (Core.Appt exp ty) = transformed_exp exp

transformed_exp a = Hs.HsLit $ Hs.HsString ("Unknown: " ++ show a)

transformed_alt (Core.Acon qdcon tbinds vbinds exp) = Hs.HsAlt nowhere (Hs.HsPApp hs_qname hs_pats) (Hs.HsUnGuardedAlt $ transformed_exp exp) []
  where hs_qname = let (mname, dcon) = qdcon in Hs.UnQual $ Hs.HsIdent dcon
        hs_pats = map transformed_pat vbinds
        transformed_pat (n, _) = Hs.HsPVar $ Hs.HsIdent n
transformed_alt (Core.Alit lit exp) = Hs.HsAlt nowhere (Hs.HsPLit $ transformed_lit lit) (Hs.HsUnGuardedAlt $ transformed_exp exp) []
transformed_alt (Core.Adefault exp) = Hs.HsAlt nowhere (Hs.HsPWildCard) (Hs.HsUnGuardedAlt $ transformed_exp exp) []

-- todo: primitives? Sorts of fracs? Should we ever generate them?
transformed_lit (Core.Literal (Core.Lchar c) _) = Hs.HsChar c
transformed_lit (Core.Literal (Core.Lstring s) _) = Hs.HsString s
transformed_lit (Core.Literal (Core.Lint i) _) = Hs.HsInt i
transformed_lit (Core.Literal (Core.Lrational f) _) = Hs.HsFrac f


mname_name (Core.M ((Core.P name), _, _)) = name

main = do
  core <- coreFileContents
  let hs = hcr_to_hs core
  putStrLn $ show $ hs
  writeFileContents hs
  return ()
