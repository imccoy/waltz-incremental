import Prelude hiding (FilePath)
import Data.List (last)
import System (getArgs)

import Filesystem.Path.CurrentOS hiding (fromText)
import qualified Filesystem.Path.CurrentOS as FP
import qualified Data.Text as ST
import qualified Data.Text.Lazy as T
import Shelly

incr :: FilePath -> FilePath -> ShIO ()
incr destDirName srcCoreFileName = do
  let baseName = basename srcCoreFileName
  let incrementalizedCoreFileName = append destDirName $ append (dirname srcCoreFileName) (decodeString $ encodeString baseName ++ "-incr.hcr")
  let incrementalizedHsFileName = replaceExtension incrementalizedCoreFileName (ST.pack "hs")
  run_ (fromText $ T.pack "Incrementalizer") $ map toTextUnsafe [srcCoreFileName, incrementalizedCoreFileName]
  run_ (fromText $ T.pack "hcr2hs") $ map toTextUnsafe [incrementalizedCoreFileName, incrementalizedHsFileName]

main = do
  args <- getArgs
  let (destDirName:srcCoreFileNames) = reverse args
  shelly $ verbosely $ do
    mapM_ (incr (fromText $ T.pack destDirName)) (map decodeString srcCoreFileNames)
