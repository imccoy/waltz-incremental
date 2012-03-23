import B
import Radtime

main = do
  putStrLn $ show $ words_length ["abcdef", "abc"]
  putStrLn $ show $ let
                      prior = words_length ["abc"]
                      change = words_length_incrementalised (BuiltinList_incrementalised_build_using_1 "abcdef")
                    in
                      applyInputChange change prior
