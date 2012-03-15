import IO
import Language.Core.Parser
import Language.Core.ParseGlue
import Language.Haskell.Pretty

import Zcode

import Utils
import HcrHs

writeHsFileContents filename hs =  do
  writeFileContents filename (prettyPrint hs ++ "\n")

main = do
  (from, to) <- parseSrcDest "hcr2hs" "hcr" "hs"
  core <- coreFileContents from
  let hs = hcr_to_hs core
  putStrLn $ show $ hs
  writeHsFileContents to hs
  return ()
