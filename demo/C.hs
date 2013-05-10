module C where

f :: Int -> Int
f 0 = 1
f n = n * f (n - 1)

