module Radtime where

data OutputChange a = OutputChange String a
  deriving (Show)

applyOutputChange (OutputChange "base:GHCziNum.zp @ ghczmprim:GHCziTypes.Int base:GHCziNum.zdfNumInt" n) m = n + m
