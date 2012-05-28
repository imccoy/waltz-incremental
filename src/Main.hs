{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Builtins
import Fauxlude
import Types

import qualified Data.Map as Map
import System.IO
import System.Exit

import Safe
import Language.Core.Core
import Language.Core.Parser
import Language.Core.ParseGlue

envInsert k v (Env m) = Env $ Map.insert k v m
envLookup k@(Just (M (package, modsIds, modId)), varId) (Env m)
   | package == basePackage = builtin baseFunc
   | package == integerPackage = builtin integerFunc
   | otherwise = Map.lookup k m
   where builtin f = Just $ Thunk envEmpty (RtExp $ f modsIds modId varId) []
envLookup k (Env m) = Map.lookup k m

envFromModules :: [Module] -> Env
envFromModules mods = resultEnv
  where resultEnv = foldr envFromModule envEmpty mods
        envFromModule mod@(Module mname _ vdefs) env
           = foldr (envFromVdefg resultEnv) env vdefs
envFromVdefg resultEnv vdefg env
  = case vdefg of
      (Rec vdefs)   -> foldr envFromVdef env vdefs
      (Nonrec vdef) -> envFromVdef vdef env
  where envFromVdef (Vdef (qVar, ty, exp)) env
           = envInsert qVar (Thunk resultEnv (CoreExp exp) []) env

eval :: [Module] -> Id -> Id -> [Value] -> Value
eval mods startMod startFun args = Thunk env
                                         (CoreExp varExp)
                                         args
  where mod = moduleById startMod mods
        env = envFromModules mods
        varExp = Var (Just $ moduleMname mod , startFun)

whnf value
  | isThunkValue value = whnf $ reduce value
  | otherwise          = value

deepseq value = go $ whnf value
  where go (DataValue con args) = DataValue con $ map deepseq args
        go v                    = v

reduce thunk@(Thunk {thunkExp = (RtExp f) }) = f $ map whnf $ thunkArgs thunk
reduce thunk@(Thunk {thunkExp = (CoreExp exp)}) = go exp
 where
  go (Var qVar)           = let val = fromJustNote ("Looking for var " 
                                                      ++ show qvar) $ 
                                           envLookup qVar $ thunkEnv thunk
                             in if isThunkValue val
                                  then val { thunkArgs = thunkArgs val
                                                            ++ thunkArgs thunk }
                                  else assertNote ("losing args " ++
                                                       (show $ thunkArgs thunk))
                                                  (length (thunkArgs thunk)
                                                        == 0)
                                                  val
  go (Dcon qDcon)         = DataValue { tag = qDcon
                                      , dataArgs = thunkArgs thunk }
  go (Lit lit)            = reduceLit lit
  go (App exp arg)        = thunk { thunkArgs = (Thunk (thunkEnv thunk)
                                                       (CoreExp arg)
                                                       []):(thunkArgs thunk) 
                                  , thunkExp = CoreExp exp }
  go (Appt exp _)         = thunk { thunkExp = CoreExp exp }
  go (Lam (Vb vbind) exp) = case thunkArgs thunk of
                              arg:args -> let newEnv
                                                = envInsert (Nothing, fst vbind)
                                                            arg
                                                            (thunkEnv thunk)
                                           in Thunk newEnv (CoreExp exp) args
                              []       -> error ("no arg for " ++ 
                                                    show (thunkExp thunk))
  go (Lam (Tb _) exp)     = thunk { thunkExp = CoreExp exp }
  go (Let vdefg exp)      = let newEnv = envFromVdefg newEnv
                                                      vdefg
                                                      (thunkEnv thunk)
                             in thunk { thunkExp = CoreExp exp
                                      , thunkEnv = newEnv }
  go (Case exp vbind ty alts)
                          = let expV = whnf $ Thunk (thunkEnv thunk)
                                                    (CoreExp exp)
                                                    []
                                envWithScrut = envInsert (Nothing, fst vbind)
                                                          expV
                                                          (thunkEnv thunk)
                                alt = head $ [alt | alt <- alts
                                                  , matchingAlt alt expV]
                                newEnv = envWithAltBindings alt
                                                            expV
                                                            envWithScrut
                             in thunk { thunkExp = CoreExp $ altExp alt
                                      , thunkEnv = newEnv }
  go (Cast exp _)         = thunk { thunkExp = CoreExp exp }
  go (Note _ exp)         = thunk { thunkExp = CoreExp exp }
reduce val = val

reduceLit (Literal (Lint i) _) = IntegralValue i
reduceLit (Literal (Lchar c) _) = CharValue c
reduceLit (Literal (Lstring s) _) = StringValue s

matchingAlt :: Alt -> Value -> Bool
matchingAlt (Acon qDcon _ _ _)           (DataValue qDcon' _) = qDcon == qDcon'
matchingAlt (Alit (Literal (Lint i) _) _)  (IntegralValue i') = i == i'
matchingAlt (Alit (Literal (Lchar i) _) _) (CharValue i')     = i == i'
matchingAlt (Adefault _)       _                              = True

envWithAltBindings (Acon _ _ vbinds _) (DataValue _ args) env
  = foldr go env $ zip vbinds args
  where go (vbind, arg) env = envInsert (Nothing, fst vbind)
                                        arg
                                        env
envWithAltBindings _ _ env = env

altExp (Acon _ _ _ exp) = exp
altExp (Alit _ exp) = exp
altExp (Adefault exp) = exp

isThunkValue (Thunk {}) = True
isThunkValue _ = False


moduleMname (Module mname _ _) = mname

moduleById id mods = headNote ("Couldn't find module with id " ++ id) $
                       [ mod | mod <- mods, moduleId mod == id ]
moduleId (Module (M (_, _, id)) _ _) = id

coreFileContents filename = do
  file <- openFile filename ReadMode
  contents <- hGetContents file
  case parse contents 0 of
    (FailP e) -> do putStrLn "HORRIBLY WRONG"
                    putStrLn e
                    exitFailure
    (OkP e) -> return e

writeFileContents filename content = do
  file <- openFile filename WriteMode
  hPutStr file $ content
  hClose file

main = do
  mods <- mapM coreFileContents ["B.hcr" ]
  let result = eval mods "B" "appzustate" [eval mods "B" "initialzustate" []]
  --let result = eval mods "B" "initialzustate" []
  putStrLn "Starting"
  putStrLn $ show $ deepseq result
  return ()
