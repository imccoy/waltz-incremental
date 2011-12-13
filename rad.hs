import Control.Monad
import Data.Maybe
import Debug.Trace
import IO
import Language.Core.Core
import Language.Core.Parser
import Language.Core.ParseGlue
import System.Exit
import Zcode

data InputPerspective = BaseCase Exp
                      | InputChange (Qual Dcon) [Vbind] Exp Exp Exp
 
mutant (Module name tdefs vdefgs) = Module name tdefs vdefgs'
  where
    input_changes_in_vdefs = map input_changes $ flattenBinds vdefgs
    vdefgs' = vdefgs ++ (map Nonrec $ foldr (++) [] $ map new_toplevel_fns input_changes_in_vdefs)

input_changes vdef@(Vdef (var, ty, exp)) = (vdef, catMaybes $ concat $ input_changes_in_bind var exp)

new_toplevel_fns (vdef, changes) = map (new_toplevel_fn vdef) changes

new_toplevel_fn (Vdef (var, ty, exp)) (InputChange dcon vbinds recursive_call combiner new_value) = Vdef (append_to_name var dcon, new_fn_ty, new_toplevel_exp)
  where
    previous_value_type = return_type ty
    new_fn_ty = foldr mkFunTy previous_value_type (previous_value_type:(map snd vbinds))
    new_fn_body = App (App combiner new_value) (Var (Nothing, "previous_value"))
    new_fn_with_vbind_args = foldr (\(var, ty) body -> Lam (Vb (var, ty)) body) new_fn_body vbinds
    new_toplevel_exp = Lam (Vb $ ("previous_value", previous_value_type)) new_fn_with_vbind_args

return_type (Tapp _ ty) = ty
return_type (Tforall _ ty) = return_type ty
return_type ty = error $ "unknown type " ++ show ty ++ " for return_type"

input_changes_in_bind fn (Lam (Vb (var, ty)) exp) = (input_changes_in_arg fn var exp):(input_changes_in_bind fn exp)
input_changes_in_bind fn (Lam (Tb (tvar, kind)) exp) = input_changes_in_bind fn exp
input_changes_in_bind fn (Cast exp ty) = input_changes_in_bind fn exp
input_changes_in_bind fn (Note string exp) = input_changes_in_bind fn exp
input_changes_in_bind fn _ = []

-- todo: recursive cases should account for aliasing of var
input_changes_in_arg fn var (Case (Var (mod, v)) bind ty alts)
  | var == v = map (input_changes_in_deconstruction fn mod) alts
  | otherwise = concat [input_changes_in_arg fn var (alt_exp a) | a <- alts]
input_changes_in_arg fn var (Lam bind exp) = input_changes_in_arg fn var exp
input_changes_in_arg fn var (App exp1 exp2) = input_changes_in_arg fn var exp2
input_changes_in_arg fn var (Let vdefg exp) = input_changes_in_arg fn var exp
input_changes_in_arg fn var (Cast exp ty) = input_changes_in_arg fn var exp
input_changes_in_arg fn var (Note string exp) = input_changes_in_arg fn var exp
input_changes_in_arg fn var _ = []

input_changes_in_deconstruction fn mod (Acon dcon tbinds vbinds exp) = let vbindexp (var, _) = Var (mod, var)
                                                                           vbindexps = map vbindexp vbinds
                                                                        in do recursive_call <- find_recursive_call fn vbindexps exp
                                                                              combiner <- find_combiner exp recursive_call
                                                                              new_piece <- find_new_piece exp recursive_call combiner
                                                                              let vbinds_for_new_piece = vbinds_not_used_in_exp vbinds recursive_call
                                                                              return $ InputChange dcon vbinds_for_new_piece recursive_call combiner new_piece

find_recursive_call fn potential_args exp = find_recursive_call' exp
  where
    find_recursive_call' app@(App exp1 exp2) = let
                                                 find_matching_call (vbind:vbinds)
                                                          | is_call exp1 fn && is_arg exp2 vbind = Just app
                                                          | otherwise                            = find_matching_call vbinds
                                                 find_matching_call [] = Nothing
                                              in case find_matching_call potential_args of
                                                    Nothing -> find_recursive_call' exp2
                                                    Just x  -> Just x
    find_recursive_call' (Lam _ exp) = find_recursive_call' exp
    find_recursive_call' (Let _ exp) = find_recursive_call' exp
    find_recursive_call' (Case exp vbind _ alts) = listToMaybe $ catMaybes $ map find_recursive_call' (exp:(map alt_exp alts))
    find_recursive_call' (Cast exp _) = find_recursive_call' exp
    find_recursive_call' (Note _ exp) = find_recursive_call' exp
    find_recursive_call' _ = Nothing -- error $ "Couldn't find recursive call of " ++ show fn
    is_call (Var expvar) fn = (unqual expvar) == (unqual fn)
    is_call (Appt exp _) fn = is_call exp fn
    is_call expr fn = False
    is_arg exp2 vbind = exp2 == vbind
 
 
find_combiner exp_after_deconstruction recursive_call = find_combiner' exp_after_deconstruction
  where find_combiner' a@(App exp1 exp2)
          | exp2 `same_app` recursive_call = first_arg_to_first_app exp1
          | otherwise                      = find_combiner' exp2 `mplus` find_combiner' exp1
        find_combiner' (Appt exp _) = find_combiner' exp
        find_combiner' (Lam _ exp) = find_combiner' exp
        find_combiner' (Case exp vbind _ alts) = listToMaybe $ catMaybes $ map find_combiner' (exp:(map alt_exp alts)) 
        find_combiner' (Cast exp _) = find_combiner' exp
        find_combiner' (Note _ exp) = find_combiner' exp
        find_combiner' e = trace ("skipping " ++ exp_con e) Nothing

first_arg_to_first_app :: Exp -> Maybe Exp
first_arg_to_first_app (App exp1 _) = Just exp1
first_arg_to_first_app (Appt exp _) = first_arg_to_first_app exp
first_arg_to_first_app (Lam _ exp) = first_arg_to_first_app exp
first_arg_to_first_app (Case exp vbind _ alts) = listToMaybe $ catMaybes $ map first_arg_to_first_app (exp:(map alt_exp alts)) 
first_arg_to_first_app (Cast exp _) = first_arg_to_first_app exp
first_arg_to_first_app (Note _ exp) = first_arg_to_first_app exp
first_arg_to_first_app _ = Nothing

vbinds_not_used_in_exp :: [Vbind] -> Exp -> [Vbind]
vbinds_not_used_in_exp vbinds (Var var) = remove_var_from_vbinds var vbinds
vbinds_not_used_in_exp vbinds (App exp1 exp2) = vbinds_not_used_in_exp (vbinds_not_used_in_exp vbinds exp1) exp2
vbinds_not_used_in_exp vbinds (Appt exp _) = vbinds_not_used_in_exp vbinds exp
vbinds_not_used_in_exp vbinds (Lam _ exp) = vbinds_not_used_in_exp vbinds exp
vbinds_not_used_in_exp vbinds (Case exp vbind _ alts) = let vbinds_not_used_in_case_exp = vbinds_not_used_in_exp vbinds exp
                                                         in foldr (flip vbinds_not_used_in_exp) vbinds_not_used_in_case_exp $ map alt_exp alts
vbinds_not_used_in_exp vbinds (Cast exp _) = vbinds_not_used_in_exp vbinds exp
vbinds_not_used_in_exp vbinds (Note _ exp) = vbinds_not_used_in_exp vbinds exp

remove_var_from_vbinds var' ((vbind@(var, ty)):vbinds)
  | var == (without_module var') = remove_var_from_vbinds var' vbinds
  | otherwise                    = vbind:(remove_var_from_vbinds var' vbinds)
remove_var_from_vbinds var' [] = []
 
find_new_piece exp_after_deconstruction recursive_call combiner = find_new_piece' exp_after_deconstruction
  where find_new_piece' (App exp1 exp2)
          | exp2 `same_app` recursive_call = find_arg_in exp1
          | call_contained_in exp1         = Just exp2
        find_new_piece' (Lam _ exp) = find_new_piece' exp
        find_new_piece' (Case exp vbind _ alts) = listToMaybe $ catMaybes $ map find_new_piece' (exp:(map alt_exp alts)) 
        find_new_piece' (Cast exp _) = find_new_piece' exp
        find_new_piece' (Note _ exp) = find_new_piece' exp
        find_new_piece' (Appt exp _) = find_new_piece' exp
        find_new_piece' _ = Nothing

        find_arg_in (App exp1 exp2)
          | exp1 `same_app` combiner = Just exp2
          | otherwise                = (find_arg_in exp1) `mplus` (find_arg_in exp2)
        find_arg_in (Lam _ exp) = find_arg_in exp
        find_arg_in (Case exp vbind _ alts) = listToMaybe $ catMaybes $ map find_arg_in (exp:(map alt_exp alts)) 
        find_arg_in (Appt exp _) = find_arg_in exp
        find_arg_in (Cast exp _) = find_arg_in exp
        find_arg_in (Note _ exp) = find_arg_in exp
        find_arg_in _ = Nothing

        call_contained_in (App exp1 exp2)
          | exp1 `same_app` combiner = True
          | otherwise                = call_contained_in exp2
        call_contained_in (Lam _ exp) = call_contained_in exp
        call_contained_in (Case exp vbind _ alts) = any  (== True) $ map call_contained_in (exp:(map alt_exp alts)) 
        call_contained_in (Cast exp _) = call_contained_in exp
        call_contained_in (Appt exp _) = call_contained_in exp
        call_contained_in (Note _ exp) = call_contained_in exp
        call_contained_in _ = False
 


replace_exp haystack needle sub = replace_exp' haystack
  where
    replace_exp' exp
      | needle == exp = sub
      | otherwise     = deep_replace_exp exp
    deep_replace_exp (App exp1 exp2) = App (replace_exp' exp1) (replace_exp' exp2)
    deep_replace_exp (Appt exp ty) = Appt (replace_exp' exp) ty
    deep_replace_exp (Lam bind exp)  = Lam bind (replace_exp' exp)
    deep_replace_exp (Let vdefg exp) = Let vdefg (replace_exp' exp)
    deep_replace_exp (Case exp vbind ty alts) = Case (replace_exp' exp) vbind ty (map (alt_map_exp replace_exp') alts)
    deep_replace_exp (Cast exp ty) = Cast (replace_exp' exp) ty
    deep_replace_exp (Note string exp) = Note string (replace_exp' exp)

append_to_name (mod, name) b = (mod, name ++ (zencode $ without_module b))

exp_con (Var _) = "Var"
exp_con (Dcon _) = "Dcon"
exp_con (Lit _) = "Lit"
exp_con (App _ _) = "App"
exp_con (Appt _ _) = "Appt"
exp_con (Lam _ _) = "Lam"
exp_con (Let _ _) = "Let"
exp_con (Case _ _ _ _) = "Case"
exp_con (Cast _ _) = "Cast"
exp_con (Note _ _) = "Note"
exp_con (External _ _) = "External"

without_module = snd

same_app (Appt a _) b = a == b
same_app a (Appt b _) = a == b
same_app a b          = a == b

instance Eq Exp where
  (Var a) == (Var b) = a == b
  (Dcon a) == (Dcon b) = a == b
  (Lit a) == (Lit b) = a == b
  (App a1 a2) == (App b1 b2) = a1 == b1 && a2 == b2
  (Appt a1 a2) == (Appt b1 b2) = a1 == b1 && a2 == b2
  (Lam a1 a2) == (Lam b1 b2) = a1 == b1 && a2 == b2
  (Let a1 a2) == (Let b1 b2) = a1 == b1 && a2 == b2
  (Case a1 a2 a3 a4) == (Case b1 b2 b3 b4) = a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4
  (Cast a1 a2) == (Cast b1 b2) = a1 == b1 && a2 == b2
  a == b = False

instance Eq Alt where
  (Acon a1 a2 a3 a4) == (Acon b1 b2 b3 b4) = (a1 == b1) && (a2 == b2) && (a3 == b3) && (a4 == b4)
  (Alit a1 a2) == (Alit b1 b2) = (a1 == b1) && (a2 == b2)
  (Adefault a) == (Adefault b) = (a == b)
  _ == _ = False

instance Eq Bind where
  (Vb a) == (Vb b) = (a == b)
  (Tb a) == (Tb b) = (a == b)
  _ == _ = False

instance Eq Vdefg where
  (Rec a) == (Rec b) = (a == b)
  (Nonrec a) == (Nonrec b) = (a == b)
  _ == _ = False

instance Eq Kind where
  Klifted == Klifted = True
  Kunlifted == Kunlifted = True
  Kopen == Kopen = True
  (Karrow a1 a2) == (Karrow b1 b2) = a1 == b1 && a2 == b2
  (Keq a1 a2) == (Keq b1 b2) = a1 == b1 && a2 == b2
  _ == _ = False

instance Eq Vdef where
  (Vdef (a1, a2, a3)) == (Vdef (b1, b2, b3)) = a1 == b1 && a2 == b2 && a3 == b3

alt_exp (Acon _ _ _ exp) = exp
alt_exp (Alit _ exp) = exp
alt_exp (Adefault exp) = exp

alt_map_exp f (Acon a b c exp) = (Acon a b c (f exp))
alt_map_exp f (Alit a exp) = (Alit a (f exp))
alt_map_exp f (Adefault exp) = (Adefault (f exp))

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
