{-# LANGUAGE TupleSections, StandaloneDeriving #-}
module Types where

import Control.Monad.State
import qualified Data.Array as Array
import Data.List (intersperse)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

import Language.Core.Core
import Language.Core.Parser()


type Arity = Int
data InterpExp = CoreExp Exp
               | RtExp ([HeapValue] -> HeapM Value) String Arity

instance Show InterpExp where
  show (CoreExp exp) = show exp
  show (RtExp _ name _) = "RtExp " ++ name

instance Eq InterpExp where
  (CoreExp exp) == (CoreExp exp2)  = exp == exp2
  (RtExp _ s n) == (RtExp _ s2 n2) = s == s2 && n == n2
  _ == _ = False

type HeapValue = Integer
data Value = Thunk { thunkEnv :: Env
                   , thunkExp :: InterpExp
                   , thunkArgs :: [HeapValue] }
           | DataValue { tag :: Qual Dcon, dataArgs :: [HeapValue] }
           | IntegralValue { integralValue :: Integer }
           | CharValue { charValue :: Char }
           | StringValue { stringValue :: String }
  deriving (Eq)

deriving instance Eq Bind
deriving instance Eq Vdefg
deriving instance Eq Vdef
deriving instance Eq Kind
deriving instance Eq Alt
deriving instance Eq Exp

showValue heap (Thunk env exp args) = "Thunk\n" ++
                                        indent 6 (show exp) ++ "\n" ++
                                        showArgs heap args
showValue heap (DataValue tag args) = "DataValue " ++ show tag ++ "\n" ++
                                        showArgs heap args
showValue _ (IntegralValue n)    = "IntegralValue " ++ show n
showValue _ (CharValue c)        = "CharValue " ++ show c
showValue _ (StringValue s)      = "StringValue " ++ s

valueConstr (Thunk {}) = "Thunk"
valueConstr (DataValue { tag = con}) = "DataValue " ++ show con 
valueConstr (IntegralValue {}) = "IntegerValue"
valueConstr (CharValue {}) = "CharValue"
valueConstr (StringValue {}) = "StringValue"

showArgs heap args = indent 2 $ joinLines $ map f args
  where f arg = showValue heap $ fst $ fromJust $ fst heap Array.! arg

showValueHeap (v, h) = showValue h v

type Dependent = (HeapValue, Value) -- the value must be a thunk
type Dependents = [Dependent]
type HeapStore = Array.Array HeapValue (Maybe (Value, Dependents))
type Heap = (HeapStore, HeapValue)
type HeapM = State Heap

runHeapEmpty = runHeap emptyHeap
runHeap heap f = runState f heap

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
           = vs Array.// [(i, Just (v, []))]

heapSet i v = heapGetStore >>= heapSetStore . (Array.// [(i, Just (v, []))])
heapSetWithDeps i v d = heapGetStore >>=
                        heapSetStore . (Array.// [(i, Just (v, d))])
heapGet i = heapGetStore >>= return . fst. fromJust . (Array.! i)
heapGetWithDeps i = heapGetStore >>= return . fromJust . (Array.! i)
heapChange :: (Value -> HeapM Value) -> HeapValue -> HeapM ()
heapChange f i = do (v, d) <- heapGetWithDeps i
                    v' <- f v
                    heapSetWithDeps i v' d

dependsOn d@(_, Thunk {}) (i, reason) = heapGetStore >>= heapSetStore . f
  where f arr = let (v, deps) = fromJust $ arr Array.! i
                 in arr Array.// [(i, Just (v, add d deps))]
        add d@(heapValue, thunk) deps
         | heapValue `elem` map fst deps = deps
         | otherwise                     = d:deps 
heapGetDeps i = heapGetStore >>= return . snd. fromJust . (Array.! i)

indent :: Int -> String -> String
indent n = joinLines . map (replicate n ' ' ++) . lines

joinLines = concat . intersperse "\n"

newtype Env = Env (Map.Map (Mname, Id) HeapValue)
  deriving (Eq)
envEmpty = Env $ Map.empty
envInsert k v (Env m) = Env $ Map.insert k v m
envLookup k (Env m) = Map.lookup k m
envKeys (Env m) = Map.keys m

instance Show Env where
  show _ = "Env"

singleton :: a -> [a]
singleton = return
