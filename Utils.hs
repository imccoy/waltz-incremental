module Utils where

import Control.Monad
import IO
import System (getArgs)
import System.Exit

import Language.Core.Core
import Language.Core.Parser
import Language.Core.ParseGlue

import Zcode

import qualified Language.Haskell.Syntax as Hs

add_imports (Hs.HsModule loc name thing imports defs) additional_import_names = Hs.HsModule loc name thing (imports ++ additional_imports) defs
  where additional_imports = map additional_import additional_import_names
        additional_import name = Hs.HsImportDecl { Hs.importLoc = hs_nowhere
                                                 , Hs.importModule = Hs.Module name
                                                 , Hs.importQualified = False
                                                 , Hs.importAs = Nothing
                                                 , Hs.importSpecs = Nothing
                                                 }

merge_hs base mods = simplify_imports $ foldl merge' base mods
  where merge' (Hs.HsModule loc name thing imports defs) (Hs.HsModule loc' name' thing' imports' defs') = Hs.HsModule loc name thing (imports ++ imports') (defs ++ defs')
        simplify_imports (Hs.HsModule loc name thing imports defs) = (Hs.HsModule loc name thing (simplified_imports name imports) defs)
        simplified_imports name imports = filter (\imp -> Hs.importModule imp /= name) imports

hs_nowhere = Hs.SrcLoc "nowhere" 0 0

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

coreFileContents filename = do
  file <- openFile filename ReadMode
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

parseSrcDest name fromHint toHint = do
  args <- getArgs
  when (length args /= 2) $ do
    putStrLn $ "Usage: " ++ name ++ " <from" ++ fromHint ++ "> <to" ++ toHint ++ ">"
    exitFailure
  let (from:to:[]) = args
  return (from, to)

