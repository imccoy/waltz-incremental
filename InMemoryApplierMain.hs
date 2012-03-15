module Main where

import qualified Language.Haskell.Pretty as HsPretty
import InMemoryApplier
import Utils

main = do
  (from, to) <- parseSrcDest "InMemoryApplier" ".unincrementalized.hcr" ".instances.hs"
  core <- coreFileContents from
  let typeclass_instances_code = in_memory_applier core
  writeFileContents to $ "{-# LANGUAGE MultiParamTypeClasses #-}\n" ++ (HsPretty.prettyPrint typeclass_instances_code)
  return ()
