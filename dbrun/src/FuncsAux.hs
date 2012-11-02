module FuncsAux where

{-# NOINLINE intEq #-}
intEq :: Int -> Int -> Bool
intEq a b = a == b

{-# NOINLINE intPlus #-}
intPlus :: Int -> Int -> Int
intPlus a b = a + b
