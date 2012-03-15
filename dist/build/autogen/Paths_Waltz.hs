module Paths_Waltz (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/iain/.cabal/bin"
libdir     = "/Users/iain/.cabal/lib/Waltz-0.0/ghc-7.0.3"
datadir    = "/Users/iain/.cabal/share/Waltz-0.0"
libexecdir = "/Users/iain/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "Waltz_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "Waltz_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "Waltz_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "Waltz_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
