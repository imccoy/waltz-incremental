import Control.Monad
import Data.Maybe
import Debug.Trace
import IO
import Language.Core.Core
import Language.Core.Parser
import Language.Core.ParseGlue
import System.Exit

coreFileContents = do
  file <- openFile "B.hcr" ReadMode
  contents <- hGetContents file
  case parse contents 0 of
    (FailP e) -> do putStrLn "HORRIBLY WRONG"
                    putStrLn e
                    exitFailure
    (OkP e) -> return e
 
mutant (Module name tdefs vdefgs) = mutant_vdefgs vdefgs

mutant_vdefgs = map mutant_vdefg

mutant_vdefg (Rec vdefs) = map mutant_vdef vdefs
mutant_vdefg (Nonrec vdef) = [mutant_vdef vdef]

mutant_vdef (Vdef (var, ty, exp)) = (without_module var, mutant_toplevel_exp var exp)

mutant_toplevel_exp fn exp = input_changes fn exp

input_changes fn (Lam (Vb (var, ty)) exp) = (input_changes_in_arg fn var exp):(input_changes fn exp)
input_changes fn (Lam (Tb (tvar, kind)) exp) = input_changes fn exp
input_changes fn (Cast exp ty) = input_changes fn exp
input_changes fn (Note string exp) = input_changes fn exp
input_changes fn _ = []

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
                                                                              return (recursive_call, combiner)

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
    find_recursive_call' _ = Nothing
    is_call (Var expvar) fn = (unqual expvar) == (unqual fn)
    is_call (Appt exp _) fn = is_call exp fn
    is_call expr fn = False
    is_arg exp2 vbind = exp2 == vbind
 
 
find_combiner exp_after_deconstruction recursive_call = find_combiner' exp_after_deconstruction
  where find_combiner' a@(App exp1 exp2)
          | exp2 `same_app` recursive_call = function_identifier exp1
          | otherwise                      = find_combiner' exp2 `mplus` find_combiner' exp1
        find_combiner' (Appt exp _) = find_combiner' exp
        find_combiner' (Lam _ exp) = find_combiner' exp
        find_combiner' (Case exp vbind _ alts) = listToMaybe $ catMaybes $ map find_combiner' (exp:(map alt_exp alts)) 
        find_combiner' (Cast exp _) = find_combiner' exp
        find_combiner' (Note _ exp) = find_combiner' exp
        find_combiner' e = trace ("skipping " ++ exp_con e) Nothing

function_identifier :: Exp -> Maybe Exp
function_identifier v@(Var _) = Just v
function_identifier (App exp1 _) = function_identifier exp1
function_identifier (Appt exp _) = function_identifier exp
function_identifier (Lam _ exp) = function_identifier exp
function_identifier (Case exp vbind _ alts) = listToMaybe $ catMaybes $ map function_identifier (exp:(map alt_exp alts)) 
function_identifier (Cast exp _) = function_identifier exp
function_identifier (Note _ exp) = function_identifier exp
function_identifier _ = Nothing

 
find_new_piece exp recursive_call combiner = find_new_piece' exp
  where find_new_piece' (App exp1 exp2)
          | exp2 == recursive_call = find_arg_in exp1
          | call_contained_in exp1 = Just exp2
        find_new_piece' (Lam _ exp) = find_new_piece' exp
        find_new_piece' (Case exp vbind _ alts) = listToMaybe $ catMaybes $ map find_new_piece' (exp:(map alt_exp alts)) 
        find_new_piece' (Cast exp _) = find_new_piece' exp
        find_new_piece' (Note _ exp) = find_new_piece' exp
        find_new_piece' _ = Nothing

        find_arg_in (App exp1 exp2)
          | exp1 == combiner = Just exp2
          | otherwise        = (find_arg_in exp1) `mplus` (find_arg_in exp2)
        find_arg_in (Lam _ exp) = find_arg_in exp
        find_arg_in (Case exp vbind _ alts) = listToMaybe $ catMaybes $ map find_arg_in (exp:(map alt_exp alts)) 
        find_arg_in (Cast exp _) = find_arg_in exp
        find_arg_in (Note _ exp) = find_arg_in exp
        find_arg_in _ = Nothing

        call_contained_in (App exp1 exp2)
          | exp1 == combiner = True
          | otherwise        = call_contained_in exp2
        call_contained_in (Lam _ exp) = call_contained_in exp
        call_contained_in (Case exp vbind _ alts) = any  (== True) $ map call_contained_in (exp:(map alt_exp alts)) 
        call_contained_in (Cast exp _) = call_contained_in exp
        call_contained_in (Note _ exp) = call_contained_in exp
        call_contained_in _ = False
 


replace_exp haystack needle sub = replace_exp' haystack
  where
    replace_exp' exp
      | needle == exp = sub
      | otherwise     = deep_replace_exp exp
    deep_replace_exp (App exp1 exp2) = App (replace_exp' exp1) (replace_exp' exp2)
    deep_replace_exp (Lam bind exp)  = Lam bind (replace_exp' exp)
    deep_replace_exp (Let vdefg exp) = Let vdefg (replace_exp' exp)
    deep_replace_exp (Case exp vbind ty alts) = Case (replace_exp' exp) vbind ty (map (alt_map_exp replace_exp') alts)
    deep_replace_exp (Cast exp ty) = Cast (replace_exp' exp) ty
    deep_replace_exp (Note string exp) = Note string (replace_exp' exp)

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

main = do
  core <- coreFileContents
  putStrLn $ show $ mutant core
  return ()
