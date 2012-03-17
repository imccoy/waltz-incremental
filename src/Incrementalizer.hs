module Incrementalizer (incrementalize, mutant_tbinds) where

import Control.Monad
import qualified Data.Data as Data
import Data.Either
import Data.Maybe
import Debug.Trace
import IO

import Language.Core.Core

import Utils
import Zcode

incrementalize = mutant

mutant (Module name tdefs vdefgs) = Module name tdefs' vdefgs'
  where tdefs' = tdefs ++ (mutant_tdefs tdefs)
        vdefgs' = vdefgs ++ (mutant_vdefgs vdefgs)

mutant_tdefs = map mutant_tdef
mutant_tdef (Data qTcon tbinds cdefs) = Data (incrementalise_name qTcon) (tbinds ++ mutant_tbinds tbinds) $ 
                                             (mutant_cdefs cdefs) ++ (mutant_cdefs_builds qTcon tbinds cdefs) ++ [hoist_cdef qTcon, replacement_cdef qTcon tbinds]
mutant_tdef (Newtype qTcon1 qTcon2 tbinds ty) = Newtype (incrementalise_name qTcon1) (incrementalise_name qTcon2) (mutant_tbinds tbinds) (mutant_ty ty)

mutant_cdefs_builds qTcon tbinds cdefs = concat $ map (mutant_cdef_builds qTcon) cdefs
mutant_cdef_builds qTcon (Constr qDcon tbinds tys) = generate_cdef_builds qTcon tys builder
  where builder tys1 ty tys2 n = [Constr (apply_to_name (++ "_build_using_" ++ (show n)) $ incrementalise_name qDcon) tbinds (tys1 ++ tys2)]

mutant_tbinds = map mutant_tbind
mutant_tbind (tvar, kind) = (incrementalise_string tvar, kind)

mutant_cdefs = map mutant_cdef
mutant_cdef (Constr dcon tbinds tys) = Constr (incrementalise_name dcon) (mutant_tbinds tbinds) (mutant_tys tys)

hoist_cdef qTcon = Constr (hoistable_type_reference $ Tcon qTcon) [] []
replacement_cdef qTcon tbinds = Constr (replacement_type_reference $ Tcon qTcon) tbinds [(Tcon qTcon)]

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
mutant_ty (UnsafeCoercion ty1 ty2) = UnsafeCoercion (mutant_ty ty1) (mutant_ty ty2)

mutant_vdefgs = map mutant_vdefg
mutant_vdefg (Rec vdefs) = Rec $ map mutant_vdef vdefs
mutant_vdefg (Nonrec vdef) = Nonrec $ mutant_vdef vdef

mutant_vdef (Vdef (qVar, ty, exp)) = Vdef (incrementalise_name qVar, mutant_ty ty, mutant_exp exp)

mutant_vbinds = map mutant_vbind
mutant_vbind (var, ty) = (zencode $ incrementalise_string var, mutant_ty ty)
mutant_bind (Vb vbind) = Vb $ mutant_vbind vbind
mutant_bind (Tb tbind) = Tb $ mutant_tbind tbind

mutant_alts ty result_ty alts = (mutant_alt_lits ty result_ty alts):(concat (map (mutant_alt ty result_ty) alts))
--mutant_alt ty (Acon qDcon tbinds [] exp) = [Acon (adjust_type_reference "replace" ty) (mutant_tbinds tbinds) [("a", Tvar "a")] (App (Dcon $ adjust_type_reference "replace" ty) exp)]
mutant_alt ty result_ty (Acon qDcon tbinds [] exp) = []
mutant_alt ty result_ty (Acon qDcon tbinds vbinds exp) = (builds [] vbinds 0) $ [recursive_case]
  where recursive_case = Acon (incrementalise_name qDcon) (mutant_tbinds tbinds) (mutant_vbinds vbinds) (mutant_exp exp)
        builds :: [Vbind] -> [Vbind] -> Int -> [Alt] -> [Alt]
        builds vbinds1 (vbind@(vbind_var,vbind_ty):vbinds2) n cons
          -- This is all about building a new value around an old one, for instance consing an element onto the head of a list.
          -- n is the index of the argument to the constructor that is being fed the old value, so in `data List a = Cons a (List a)' it would point to (List a)
          -- We require all other arguments to the constructor being incrementalised - in this case, just a - to be provided as part of the incrementalised_build constructor
          -- Consider a function length (x:xs) = 1 + length xs
          -- We need to turn this into something that will return an incrementalised value, where the "length xs" term has gone away.
          -- To achieve this, we incrementalise the RHS as usual, but we make some assignments beforehand. We define xs_incrementalised as a hoist value, and elsewhere arrange
          -- for length_incrementalised to respond to a hoist value by making the term go away. More work is required to make the right thing happen when
          -- more than one hoist value is involved.
          -- When we incrementalise the RHS, it will expect incrementalised values to exist for all the other terms in the expression (x, in this case, although it is not used in
          -- the expression). In order to make those terms available, we construct replace values for each argument provided as part of the incrementalised_build constructor and assign
          -- them to the incrementalised name of the argument.
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
mutant_alt ty result_ty (Alit lit exp) = []
mutant_alt ty result_ty (Adefault exp) = [Adefault $ mutant_exp exp]

mutant_alt_lits ty result_ty alts = Acon (replacement_type_reference ty) [] [("replace_val", ty)]
                               (Case (Var $ unqual "replace_val") ("replace_val", ty) (mutant_ty ty)
                                     (mapMaybe (mutant_alt_lit ty result_ty) alts)
                               )
mutant_alt_lit ty result_ty (Alit lit exp)                 = Just $ Alit lit                 (App (Var $ replacement_type_reference $ result_ty) exp)
mutant_alt_lit ty result_ty (Acon qDcon tbinds vbinds exp) = Just $ Acon qDcon tbinds vbinds (App (Var $ replacement_type_reference $ result_ty) exp)
mutant_alt_lit ty result_ty (Adefault exp)                 = Just $ Adefault                 (App (Var $ replacement_type_reference $ result_ty) exp)

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
                                                                                 Acon (hoistable_type_reference $ snd vbind) [] [] 
                                                                                      (Var $ identity_type_reference $ param_exp_type),
                                                                                 Adefault (mutant_exp exp) ]
                                                                               where (Vb newBind) = mutant_bind $ Vb vbind
                                    Left e -> Lam (mutant_bind $ Vb vbind) (mutant_exp exp)
mutant_exp (Let vdefg exp) = Let (mutant_vdefg vdefg) (mutant_exp exp)
mutant_exp (Case exp vbind ty alts) = Case (mutant_exp exp) (mutant_vbind vbind) (mutant_ty ty) (mutant_alts (snd vbind) ty alts)
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

unsafe_type_of_exp a = case type_of_exp a of
                         Right a -> a
                         Left e -> error e



