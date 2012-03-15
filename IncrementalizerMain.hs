import Incrementalizer
import Utils

main = do
  core <- coreFileContents "B.hcr"
  let mutant_core = incrementalize core
  putStrLn $ show $ mutant_core
  writeFileContents "Bprime.hcr" $ show mutant_core
  return ()
