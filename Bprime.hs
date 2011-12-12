module B where
words_length1rlq
  = \ dsdli ->
      case dsdli of
          ([]) -> 0
          (:) wabu wsabv -> (((+) (length wabu)) (words_length1rlq wsabv))
words_length = words_length1rlq
words_length1rlqZC
  = \ previous_value ->
      \ wabu -> (((+) (length wabu)) previous_value)
