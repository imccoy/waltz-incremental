import Control.Monad ( when )
import System ( getArgs )
import System.Exit
import System.IO
import System.FilePath
import System.Time ( getClockTime )

import Config
import DynFlags
import GHC
import GHC.Paths ( libdir )
import HscTypes ( Target (..) )
import DynFlags ( defaultDynFlags )
import Outputable ( showSDoc )
import StringBuffer ( stringToStringBuffer )

import Language.Core.Parser
import Language.Core.ParseGlue
import Language.Haskell.Pretty ( prettyPrint )

import DatabaseApplier
import HcrHs
import Incrementalizer
import InMemoryApplier
import Utils


performCompilation args targets = defaultErrorHandler defaultDynFlags $ do
  runGhc (Just libdir) $ do
    sdflags <- getSessionDynFlags
    (dflags, fileargs', _) <- parseDynamicFlags sdflags (map noLoc args)
    setSessionDynFlags dflags
    let fileargs = map unLoc fileargs'
    targets' <- maybe (mapM (flip guessTarget Nothing) fileargs) return targets
    setTargets targets'
    load LoadAllTargets
    return targets'

initialCompilation args = performCompilation args Nothing

fileFromTarget target = fileFromTargetId $ targetId target
  where fileFromTargetId (TargetModule moduleName) = error "can't cope with module names yet"
        fileFromTargetId (TargetFile filePath _) = filePath

coreFilename name
  | takeExtension name == ".hs" = replaceExtension name ".hcr"
  | otherwise                   = error $ "Can't get coreFilename for " ++ name

augment core hs = let db_appliers = db_applier core
                      in_memory_appliers = in_memory_applier core
                   in merge_hs hs [db_appliers, in_memory_appliers]

incrementalizeTarget target = do
  let filename = fileFromTarget target
  original_core <- coreFileContents $ coreFilename filename
  let incrementalized_core = incrementalize original_core
  let incrementalized_haskell = augment original_core $ add_imports (hcr_to_hs incrementalized_core) ["Radtime"]
  putStrLn $ prettyPrint incrementalized_haskell
  incrementalized_haskell_src <- stringToStringBuffer $ prettyPrint incrementalized_haskell
  now <- getClockTime
  return $ target { targetContents = Just (incrementalized_haskell_src, now)  }
 
main = do
  args <- getArgs
  when (elem "-V" args || elem "--version" args || elem "--numeric-version" args) $ do
    putStrLn cProjectVersion
    exitWith ExitSuccess
  when (elem "--supported-languages" args) $ do
    mapM_ putStrLn supportedLanguagesAndExtensions
    exitWith ExitSuccess
  when (elem "--print-libdir" args) $ do
    putStrLn libdir
    exitWith ExitSuccess
  targets <- initialCompilation ("-fext-core":args)
  targets' <- mapM incrementalizeTarget targets
  performCompilation args (Just targets')

