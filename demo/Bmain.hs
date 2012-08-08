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
  [show $ app_state prior
  ,show $ applyInputChange (app_state_incrementalised prior change) 
                           (app_state prior)]
  where
    prior = [NewWordInput "Dog", NewDefinitionInput "Dog" "A Wolfish Beast"]
    change = BuiltinList_incrementalised_build_using_1 $
                NewDefinitionInput "Dog" "A Loyal Housepet"
    post = (NewDefinitionInput "Dog" "A Loyal Housepet"):prior 

