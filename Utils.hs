module Utils where

import IO
import System.Exit

import Language.Core.Core
import Language.Core.Parser
import Language.Core.ParseGlue

import Zcode


generate_cdef_builds qTcon tys builder = builds [] tys 0
  where builds tys1 (ty:tys2) n 
          | ty_matches qTcon ty = (builder tys1 ty tys2 n) ++ (builds (tys1 ++ [ty]) tys2 (n+1))
          | otherwise           = builds (tys1 ++ [ty]) tys2 (n+1)
        builds _ _ _ = []

ty_matches (needle_module, needle) (Tcon (t_module, t))      = t == needle
ty_matches qTcon (Tapp bind t) = ty_matches qTcon bind
ty_matches qTcon t             = False

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

writeFileContents filename content =  do
  file <- openFile filename WriteMode
  hPutStr file $ content
  hClose file


