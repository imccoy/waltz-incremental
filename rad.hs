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

mutant_vdef (Vdef (var, ty, exp)) = (var, mutant_toplevel_exp var exp)

mutant_toplevel_exp fn exp = input_changes fn exp

input_changes fn (Lam (Vb (var, ty)) exp) = (input_changes' fn var exp):(input_changes fn exp)
input_changes fn (Cast exp ty) = input_changes fn exp
input_changes fn (Lam (Tb (tvar, kind)) exp) = input_changes fn exp
input_changes fn (Note string exp) = input_changes fn exp
input_changes fn _ = []

-- todo: recursive cases should account for aliasing of var
input_changes' fn var (Case (Var (mod, v)) bind ty alts)
  | var == v = map (input_changes_alt fn mod) alts
  | otherwise = concat [input_changes' fn var (alt_exp a) | a <- alts]
input_changes' fn var (Lam bind exp) = input_changes' fn var exp
input_changes' fn var (App exp1 exp2) = input_changes' fn var exp2
input_changes' fn var (Let vdefg exp) = input_changes' fn var exp
input_changes' fn var (Cast exp ty) = input_changes' fn var exp
input_changes' fn var (Note string exp) = input_changes' fn var exp
input_changes' fn var _ = []

input_changes_alt fn mod (Acon dcon tbinds vbinds exp) = find_recursive_call fn vbindexps exp
  where vbindexp (var, _) = Var (mod, var)
        vbindexps = map vbindexp vbinds

find_recursive_call fn potential_args exp = ("recursive_call_of", without_module fn, find_recursive_call' exp)
  where

    find_recursive_call' :: Exp -> Maybe (Exp, Exp)
    find_recursive_call' app@(App exp1 exp2) = let
                                                 find_matching_call (vbind:vbinds)
                                                          | is_call exp1 fn && is_arg exp2 vbind = Just (vbind, app)
                                                          | otherwise                            = find_matching_call vbinds
                                                 find_matching_call [] = Nothing
                                              in case find_matching_call potential_args of
                                                    Nothing -> find_recursive_call' exp2
                                                    Just x  -> Just x
    find_recursive_call' (Lam _ exp) = find_recursive_call' exp
    find_recursive_call' (Let _ exp) = find_recursive_call' exp
    find_recursive_call' (Case exp vbind _ alts) = listToMaybe $ catMaybes $ map (\x -> trace ("c" ++ show x) $ find_recursive_call' x) (exp:(map alt_exp alts))
    find_recursive_call' (Cast exp _) = find_recursive_call' exp
    find_recursive_call' (Note _ exp) = find_recursive_call' exp
    find_recursive_call' _ = Nothing
    is_call (Var expvar) fn = (unqual expvar) == (unqual fn)
    is_call (Appt exp _) fn = is_call exp fn
    is_call expr fn = False
    is_arg exp2 vbind = exp2 == vbind
 

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

instance Eq Exp where
  (Var a) == (Var b) = a == b
  (Dcon a) == (Dcon b) = a == b
  (Lit a) == (Lit b) = a == b
  (App a1 a2) == (App b1 b2) = a1 == b1 && a2 == b2
  (Lam a1 a2) == (Lam b1 b2) = a1 == b1 && a2 == b2
  (Let a1 a2) == (Let b1 b2) = a1 == b1 && a2 == b2
  (Case a1 a2 a3 a4) == (Case b1 b2 b3 b4) = a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4
  (Cast a1 a2) == (Cast b1 b2) = a1 == b1 && a2 == b2
  _ == _ = False

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
