module B where

words_length [] = 0
words_length (w:ws) = (length w) + (words_length ws)

-- words_length [] = 0
-- words_length (w:ws) = (words_length ws) + (length w)
