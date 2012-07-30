{-# LANGUAGE StandaloneDeriving #-}

import B
import Inctime

deriving instance Show AppState
deriving instance Show Input
deriving instance Show Input_incrementalised
deriving instance Show WordDefinitions
deriving instance Show WordDefinitions_incrementalised

main = do
  mapM putStrLn tests
tests =
  [show $ wordsFromInputs prior
  ,show $ applyInputChange (wordsFromInputs_incrementalised prior change)
                           (wordsFromInputs prior)
  ,show $ newDefinitionInputs prior
  ,show $ newDefinitionInputs_incrementalised prior change
  ,show $ definitionInputsFor "Dog" prior
  ,show $ definitionInputsFor_incrementalised "Dog" mkIncrementalisedIdentity prior change
  ,show $ definitionsFromInputsFor "Dog" prior
  ,show $ definitionsFromInputsFor_incrementalised "Dog" mkIncrementalisedIdentity prior change
  ,show $ wordDefinitions "Dog" prior
  ,show $ wordDefinitions_incrementalised "Dog" mkIncrementalisedIdentity prior change
  ,show $ applyInputChange (wordDefinitions_incrementalised "Dog"
                                mkIncrementalisedIdentity prior change)
                           (wordDefinitions "Dog" prior)
  ,show $ app_state prior
  ,show $ applyInputChange (app_state_incrementalised prior change) 
                           (app_state prior)]
  where
    prior = [NewWordInput "Dog", NewDefinitionInput "Dog" "A Wolfish Beast"]
    change = BuiltinList_incrementalised_build_using_1 $
                NewDefinitionInput "Dog" "A Loyal Housepet"
    post = (NewDefinitionInput "Dog" "A Loyal Housepet"):prior 

