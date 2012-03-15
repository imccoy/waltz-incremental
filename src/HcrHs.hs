module HcrHs (hcr_to_hs) where

import Char
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

import Zcode

nowhere = Hs.SrcLoc "nowhere" 0 0

hcr_to_hs (Core.Module (Core.M (Core.P name, ids, id)) tdefs vdefgs) = Hs.HsModule nowhere (Hs.Module id) Nothing [] $ (map transformed_tdef tdefs) ++ (transformed_vdefgs vdefgs)

transformed_vdefgs vdefgs = concat $ map transformed_vdefg vdefgs
 where transformed_vdefg (Core.Rec vdefs) = map transformed_vdef vdefs
       transformed_vdefg (Core.Nonrec vdef) = [transformed_vdef vdef]

transformed_vdef (Core.Vdef ((_, name), ty, exp)) = Hs.HsFunBind $ [Hs.HsMatch nowhere (Hs.HsIdent $ pzdecode name) [] (Hs.HsUnGuardedRhs $ transformed_exp exp) []]

transformed_tdef (Core.Data tcon tbinds cdefs) = Hs.HsDataDecl nowhere [] (Hs.HsIdent $ snd tcon) (map (Hs.HsIdent . fst) tbinds) (map transformed_cdef cdefs ) []
  where transformed_cdef (Core.Constr dcon tbinds tys) = Hs.HsConDecl nowhere (Hs.HsIdent $ zdecode $ snd dcon) $ map Hs.HsUnBangedTy $ map transformed_ty tys

transformed_ty (Core.Tvar tvar) = Hs.HsTyVar $ Hs.HsIdent $ tvar
transformed_ty (Core.Tapp tvar1 tvar2) = Hs.HsTyApp (transformed_ty tvar1) (transformed_ty tvar2)
transformed_ty (Core.Tcon tcon) = Hs.HsTyCon $ Hs.UnQual $ Hs.HsIdent $ zdecode $ snd tcon

-- Note String Exp	 
-- External String Ty
transformed_exp (Core.Var name) = Hs.HsVar $ simplify $ transformed_name name
transformed_exp (Core.Dcon dcon) = Hs.HsCon $ simplify $ transformed_name dcon
transformed_exp (Core.Lit lit) = Hs.HsLit $ transformed_lit lit
transformed_exp (Core.App exp1 exp2)
  | is_typeclass_specifier exp2        = transformed_exp exp1
  | is_prim_constructor exp1    = transformed_exp exp2
  | otherwise                          = Hs.HsParen $ Hs.HsApp (transformed_exp exp1) (transformed_exp exp2) 
-- transformed_exp (Core.Appt exp ty) = Hs.HsExpTypeSig nowhere (transformed_exp exp) (transformed_ty ty)
transformed_exp (Core.Lam (Core.Vb (var, _)) exp) = Hs.HsLambda nowhere [Hs.HsPVar $ Hs.HsIdent $ pzdecode var] (transformed_exp exp)
transformed_exp (Core.Lam (Core.Tb _) exp) = transformed_exp exp
transformed_exp (Core.Case exp vbind ty alts) = Hs.HsParen $ Hs.HsCase (transformed_exp exp) $ map transformed_alt alts
transformed_exp (Core.Cast exp ty) = transformed_exp exp
transformed_exp (Core.Appt exp ty) = transformed_exp exp
transformed_exp (Core.Let vdefg exp) = Hs.HsLet (transformed_vdefgs [vdefg]) (transformed_exp exp)

transformed_exp a = Hs.HsLit $ Hs.HsString ("Unknown: " ++ show a)

transformed_alt (Core.Acon qdcon tbinds vbinds exp) = Hs.HsAlt nowhere (Hs.HsPApp hs_qname hs_pats) (Hs.HsUnGuardedAlt $ transformed_exp exp) []
  where hs_qname = simplify $ transformed_name qdcon
        hs_pats = map transformed_pat vbinds
        transformed_pat (n, _) = Hs.HsPVar $ Hs.HsIdent n
transformed_alt (Core.Alit lit exp) = Hs.HsAlt nowhere (Hs.HsPLit $ transformed_lit lit) (Hs.HsUnGuardedAlt $ transformed_exp exp) []
transformed_alt (Core.Adefault exp) = Hs.HsAlt nowhere (Hs.HsPWildCard) (Hs.HsUnGuardedAlt $ transformed_exp exp) []

transformed_name ((Just mname), name) = Hs.Qual (Hs.Module $ mname_name mname) (Hs.HsIdent name)
transformed_name (Nothing, name) = Hs.UnQual (Hs.HsIdent name)

pzdecode s = let s' = zdecode s
              in case all (\c -> isAlphaNum c || c == '_') s' of
                  True -> s'
                  False -> "(" ++ s' ++ ")"

simplify (Hs.Qual mod (Hs.HsIdent name))
  | mod == (Hs.Module "base") || mod == (Hs.Module "ghczmprim") || mod == (Hs.Module "main") = Hs.UnQual (Hs.HsIdent $ pzdecode name)
  | otherwise                                                   = Hs.Qual mod (Hs.HsIdent $ pzdecode name)
simplify (Hs.UnQual (Hs.HsIdent name))                          = Hs.UnQual (Hs.HsIdent $ pzdecode name)

-- todo: primitives? Sorts of fracs? Should we ever generate them?
transformed_lit (Core.Literal (Core.Lchar c) _) = Hs.HsChar c
transformed_lit (Core.Literal (Core.Lstring s) _) = Hs.HsString s
transformed_lit (Core.Literal (Core.Lint i) _) = Hs.HsInt i
transformed_lit (Core.Literal (Core.Lrational f) _) = Hs.HsFrac f

is_typeclass_specifier (Core.Var (_, name)) = take 3 name == "zdf"
is_typeclass_specifier _ = False

is_prim_constructor (Core.Dcon (Just (Core.M (Core.P name, ["GHC"], "Types")), n)) = (n == "Izh")
is_prim_constructor _                                                              = False

mname_name (Core.M ((Core.P name), _, _)) = name

