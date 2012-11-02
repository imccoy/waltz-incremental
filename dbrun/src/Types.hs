{-# LANGUAGE TupleSections, FlexibleInstances, UndecidableInstances #-}
module Types where

import Control.Monad.State
import qualified Data.Array as Array
import Data.List (intersperse)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

import Language.Core.Core (Exp, Qual, Dcon, Mname, Id)
import Language.Core.Parser()


type Arity = Int
data InterpExp = CoreExp Exp
               | RtExp ([HeapValue] -> HeapM Value) String Arity

instance Show InterpExp where
  show (CoreExp exp) = show exp
  show (RtExp _ name _) = "RtExp " ++ name

type HeapValue = Integer
data Value = Thunk { thunkEnv :: Env
                   , thunkExp :: InterpExp
                   , thunkArgs :: [HeapValue] }
           | DataValue { tag :: Qual Dcon, dataArgs :: [HeapValue] }
           | IntegralValue { integralValue :: Integer }
           | CharValue { charValue :: Char }
           | StringValue { stringValue :: String }

showValue heap (Thunk env exp args) = "Thunk\n" ++
                                        indent 6 (show exp) ++ "\n" ++
                                        showArgs heap args
showValue heap (DataValue tag args) = "DataValue " ++ show tag ++ "\n" ++
                                        showArgs heap args
showValue _ (IntegralValue n)    = "IntegralValue " ++ show n
showValue _ (CharValue c)        = "CharValue " ++ show c
showValue _ (StringValue s)      = "StringValue " ++ s

showValueShort (Thunk env exp args) = "Thunk " ++ show exp
showValueShort (DataValue tag args) = "DataValue " ++ show tag
showValueShort (IntegralValue n)    = "IntegralValue " ++ show n
showValueShort (CharValue c)        = "CharValue " ++ show c
showValueShort (StringValue s)      = "StringValue " ++ s



showArgs heap args = indent 2 $ joinLines $ map f args
  where f arg = showValue heap $ fromJust $ fst heap Array.! arg

showValueHeap (v, h) = showValue h v


addThunkArgs :: [HeapValue] -> Value -> HeapM Value
addThunkArgs [] t = return t
addThunkArgs a t@(Thunk {thunkArgs = a'}) = return $ t { thunkArgs = a' ++ a }
addThunkArgs a v = badArgs "addThunkArgs" =<< liftM (v:) (mapM heapGet a)



type HeapStore = Array.Array HeapValue (Maybe Value)
type Heap = (HeapStore, HeapValue)
type HeapM = State Heap

runHeap f = runState f emptyHeap 

heapGetFull :: HeapM Heap
heapGetFull = get

heapGetStore :: HeapM HeapStore
heapGetStore = get >>= return . fst

heapSetStore :: HeapStore -> HeapM ()
heapSetStore x = get >>= (\(_, b) -> put (x, b))

emptyHeap = (Array.listArray (0, 1024) (repeat Nothing), 0)

heapAdd :: Value -> HeapM Integer
heapAdd v = do (vs, i) <- get
               let vs' = include vs i v
               put (vs', i + 1)
               return i
  where include vs i v 
           | snd (Array.bounds vs) <= i
           = include (Array.listArray (0, 2 * (snd $ Array.bounds vs)) 
                                      (Array.elems vs ++ repeat Nothing))
                     i
                     v
           | otherwise
           = vs Array.// [(i, Just v)]

heapSet i v = heapGetStore >>= heapSetStore . (Array.// [(i, Just v)])
heapGet :: HeapValue -> HeapM Value
heapGet i = heapGetStore >>= return . fromJust . (Array.! i)
heapChange :: (Value -> HeapM Value) -> HeapValue -> HeapM ()
heapChange f i = heapGet i >>= f >>= (heapSet i)



indent :: Int -> String -> String
indent n = joinLines . map (replicate n ' ' ++) . lines

joinLines = concat . intersperse "\n"

newtype Env = Env (Map.Map (Mname, Id) HeapValue)
envEmpty = Env $ Map.empty
envInsert k v (Env m) = Env $ Map.insert k v m
envLookup k (Env m) = Map.lookup k m
envKeys (Env m) = Map.keys m

instance Show Env where
  show _ = "Env"

singleton :: a -> [a]
singleton = return

badArgs :: String -> [Value] -> HeapM a
badArgs n [] = error $ n ++ " not defined for []"
badArgs n args = heapGetFull >>= (\h -> error $ n ++ " not defined for "
                                            ++ concat (map (showValue h) args))


