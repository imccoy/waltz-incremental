module Types where

import Data.List (intersperse)
import qualified Data.Map as Map
import Language.Core.Core (Exp, Qual, Dcon, Mname, Id)
import Language.Core.Parser()

data InterpExp = CoreExp Exp
               | RtExp ([Value] -> Value)

instance Show InterpExp where
  show (CoreExp exp) = show exp
  show (RtExp _) = "RtExp"

data Value = Thunk { thunkEnv :: Env
                   , thunkExp :: InterpExp
                   , thunkArgs :: [Value] }
           | DataValue { tag :: Qual Dcon, dataArgs :: [Value] }
           | IntegralValue { integralValue :: Integer }
           | CharValue { charValue :: Char }
           | StringValue { stringValue :: String }

instance Show Value where
  show (Thunk env exp args) = "Thunk\n" ++
                                  indent 6 (show exp) ++
                                  (indent 2 $ joinLines $ map show args)
  show (DataValue tag args) = "DataValue " ++ show tag ++ "\n" ++
                                  (indent 2 $ joinLines $ map show args)
  show (IntegralValue n)    = "IntegralValue " ++ show n
  show (CharValue c)        = "CharValue " ++ show c
  show (StringValue s)      = "StringValue " ++ s

indent :: Int -> String -> String
indent n = joinLines . map (replicate n ' ' ++) . lines

joinLines = concat . intersperse "\n"

newtype Env = Env (Map.Map (Mname, Id) Value)
envEmpty = Env $ Map.empty

instance Show Env where
  show _ = "Env"


