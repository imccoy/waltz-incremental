import B
import Radtime

main = do
  putStrLn $ show $ words_length ["abcdef", "abc"]
  putStrLn $ show $ let
                      prior = words_length ["abc"]
                      change = words_length_incrementalised (InputChangewordszulengthZZC "abcdef")
                    in
                      applyOutputChange change prior
