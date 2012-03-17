import Prelude hiding (FilePath, concat)
import Data.List (last)
import qualified Data.List as L
import Data.Maybe
import Debug.Trace
import System (getArgs)

import Filesystem.Path.CurrentOS hiding (fromText)
import qualified Filesystem.Path.CurrentOS as FP
import qualified Data.Text as ST
import qualified Data.Text.Lazy as T
import Shelly

incr :: FilePath -> FilePath -> ShIO ()
incr destDirName srcCoreFileName = do
  let baseName = basename srcCoreFileName
  let incrementalizedCoreFileName = append destDirName $ append (directory srcCoreFileName) (decodeString $ encodeString baseName ++ "-incr.hcr")
  let incrementalizedHsFileName = replaceExtension incrementalizedCoreFileName (ST.pack "hs")
  liftIO $ putStrLn $ show (srcCoreFileName, baseName, incrementalizedCoreFileName, incrementalizedHsFileName)
  run_ (fromText $ T.pack "Incrementalizer") $ map toTextUnsafe [srcCoreFileName, incrementalizedCoreFileName]
  run_ (fromText $ T.pack "hcr2hs") $ map toTextUnsafe [incrementalizedCoreFileName, incrementalizedHsFileName]

expandFileNames fileNames = mapM expandFileName fileNames >>= return . L.concat

expandFileName fileName = do
  isDir <- test_d fileName
  if isDir
    then expandDir fileName
    else return [fileName]

expandDir dirName = do
  paths <- find dirName
  cwd <- pwd >>= (\x -> return $ T.append (toTextUnsafe x) (T.pack "/"))
  return $ map (relativize cwd) $ filter (\x -> FP.extension x == (Just $ ST.pack "hcr")) paths
  where relativize cwd fileName = fromText $ fromJust $ T.stripPrefix cwd (toTextUnsafe fileName)

main = do
  args <- getArgs
  let args' = if length args == 1 then ".":args else args
  let (destDirName:srcCoreFileNames) = reverse args'
  shelly $ verbosely $ do
    srcCoreFileNames' <- expandFileNames $ map decodeString $ srcCoreFileNames
    mapM_ (incr (fromText $ T.pack destDirName)) srcCoreFileNames'
