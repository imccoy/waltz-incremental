module Main where

import DatabaseApplier
import qualified Language.Haskell.Pretty as HsPretty
import Utils


main = do
  (from, to) <- parseSrcDest "DatabaseApplier" ".unincrementalized.hcr" ".dbinstances.hs"
  core <- coreFileContents from
  let typeclass_instances_code = db_applier core
  writeFileContents to $ (HsPretty.prettyPrint typeclass_instances_code)
  return ()
