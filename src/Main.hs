{-# LANGUAGE ScopedTypeVariables, ViewPatterns, TupleSections #-}
module Main where

import System.Environment
import System.IO
import System.Exit

import Data.Map (Map)
import qualified Data.Map as Map

import Language
import Prep

import Language.Core.Parser
import Language.Core.ParseGlue
import qualified Language.Core.Core as C

data Invocation = Invocation { invocationFunction :: Qual Var, 
                               invocationArgs :: [Value],
                               invocationResult :: Value)

data ValueData = InvocationValue Invocation
               | DataValue (Qual Dcon) [Value]
               | LiteralValue

type Value = Integer
data ValueStore = Map Value ValueData

eval vdefgs exp = eval' Map.empty Map.empty vdefgs exp

eval' vs env mods exp = go exp
  where go (Var (mod, var))
          | Just a <- Map.lookup env (mod, var)
            = a
          | Just a <- ----aaaaaargh

getVdefgs (C.Module _ _ vdefgs) = vdefgs

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
  mods <- mapM coreFileContents =<< getArgs
  let mods' = map (lambdaLiftVdefgs . etaExpandVdefgs . 
                     convertVdefgs . getVdefgs)
                  mods
  putStrLn $ show mods'
  let state = eval mods' (App (Var ("B", "app_state"))
                          []
                          [(Var ("B", "intial_state"))])
  return ()
