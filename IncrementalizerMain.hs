import Incrementalizer
import Utils

main = do
  core <- coreFileContents
  let mutant_core = mutant core
  putStrLn $ show $ mutant_core
  writeFileContents "Bprime.hcr" $ show mutant_core
  return ()
