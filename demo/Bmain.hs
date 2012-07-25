{-# LANGUAGE StandaloneDeriving #-}

import B
import Inctime

deriving instance Show AppState

main = do
  mapM putStrLn tests
tests =
  [show $ words_length ["abcdef", "abc"]
  ,show $ applyInputChange (words_length_stringy_incrementalised
                             change) 
                          (words_length prior)
 
  ,"abcdef"
  ,applyInputChange (head_incrementalised change)
                    (head prior)
  
  ,show $ app_state ["abcdef", "abc"]
  ,show $ applyInputChange (app_state_incrementalised change)
                           (app_state prior)]
  where
    prior = ["abc"]
    change = BuiltinList_incrementalised_build_using_1 "abcdef"

