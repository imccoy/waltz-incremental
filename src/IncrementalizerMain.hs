import Incrementalizer
import Utils

main = do
  (from, to) <- parseSrcDest "Incrementalizer" "hcr" "hcr"
  core <- coreFileContents from
  let mutant_core = incrementalize core
  writeFileContents to $ show mutant_core
  return ()
