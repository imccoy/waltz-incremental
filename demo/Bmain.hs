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
  ["# wordsFromInputs before and after"
  ,show $ wordsFromInputs prior
  ,show $ applyInputChange (wordsFromInputs_incrementalised prior change)
                           (wordsFromInputs prior)
  ,"# newDefinitionInputs before and after"
  ,show $ newDefinitionInputs prior
  ,show $ newDefinitionInputs_incrementalised prior change
  ,"# definitionInputsFor \"Dog\" before and change"
  ,show $ definitionInputsFor "Dog" prior
  ,show $ definitionInputsFor_incrementalised "Dog" mkIncrementalisedIdentity prior change
  ,"# definitionsFromInputsFor \"Dog\" before and change"
  ,show $ definitionsFromInputsFor "Dog" prior
  ,show $ definitionsFromInputsFor_incrementalised "Dog" mkIncrementalisedIdentity prior change
  ,"# wordDefinitions \"Dog\" before, change, and after"
  ,show $ wordDefinitions prior "Dog"
  ,show $ wordDefinitions_incrementalised prior change "Dog" mkIncrementalisedIdentity
  ,show $ applyInputChange (wordDefinitions_incrementalised prior change
                                "Dog" mkIncrementalisedIdentity)
                           (wordDefinitions prior "Dog")
  ,"# definitions before and change"
  ,show $ definitions prior
  ,show $ definitions_incrementalised prior change
  ,"# app_state before and after"
  ,show $ app_state prior
  ,show $ applyInputChange (app_state_incrementalised prior change) 
                           (app_state prior)]
  where
    prior = [NewWordInput "Dog", NewDefinitionInput "Dog" "A Wolfish Beast"]
    change = BuiltinList_incrementalised_build_using_1 $
                NewDefinitionInput "Dog" "A Loyal Housepet"
    post = (NewDefinitionInput "Dog" "A Loyal Housepet"):prior 

