{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, MultiParamTypeClasses, UndecidableInstances #-}

module DbRadtime where

import Radtime

type DbAddress = String

class DbIncrementalised incrementalised base where
  applyDbInputChange :: incrementalised -> DbAddress -> IO ()

appendAddress :: DbAddress -> String -> Int -> DbAddress
appendAddress address constructor fieldIndex = address ++ " " ++ newPiece
  where newPiece = constructor ++ "." ++ (show fieldIndex)

instance DbIncrementalised Char_incrementalised Char where
  applyDbInputChange change address = error "not implemented"

instance DbIncrementalised Int_incrementalised Int where
  applyDbInputChange change address = error "not implemented"

instance (DbIncrementalised elem_incrementalised elem) => 
            DbIncrementalised (ZMZN_incrementalised elem elem_incrementalised) ([elem]) where
  applyDbInputChange change address = error "not implemented"
