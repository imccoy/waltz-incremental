module B where

words_length [] = 0
words_length (w:ws) = (length w) + (words_length ws)

words_length_doubled x = 2 * words_length x
