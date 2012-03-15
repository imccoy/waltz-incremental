import IO
import Control.Monad
import Language.Core.Parser
import Language.Core.ParseGlue
import Language.Haskell.Pretty
import System (getArgs)
import System.Exit

import Zcode

import HcrHs

coreFileContents filename = do
  file <- openFile filename ReadMode
  contents <- hGetContents file
  case parse contents 0 of
    (FailP e) -> do putStrLn "HORRIBLY WRONG"
                    putStrLn e
                    exitFailure
    (OkP e) -> return e

writeFileContents filename hs =  do
  file <- openFile filename WriteMode
  hPutStr file $ prettyPrint hs
  hPutStr file "\n"
  hClose file

main = do
  args <- getArgs
  when (length args /= 2) $ do
    putStrLn "Usage: hcr2hs <from>.hcr <to>.hs"
    exitFailure
  let (from:to:[]) = args
  convert_file from to

convert_file from to = do
  core <- coreFileContents from
  let hs = hcr_to_hs core
  putStrLn $ show $ hs
  writeFileContents to hs
  return ()
