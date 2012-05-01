{-# LANGUAGE StandaloneDeriving #-}

import B
import Inctime

deriving instance Show AppState

main = do
  putStrLn $ show $ words_length ["abcdef", "abc"]
  putStrLn $ show $ applyInputChange (words_length_stringy_incrementalised
                                        change) 
                                     (words_length prior)
 
  putStrLn $ "abcdef"
  putStrLn $ applyInputChange (head_incrementalised change)
                              (head prior)
  
  putStrLn $ show $ app_state ["abcdef", "abc"]
  putStrLn $ show $ applyInputChange (app_state_incrementalised change)
                                     (app_state prior)
  where
    prior = ["abc"]
    change = BuiltinList_incrementalised_build_using_1 "abcdef"

