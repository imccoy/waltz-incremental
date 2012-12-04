{-# LANGUAGE TupleSections, FlexibleInstances, UndecidableInstances, StandaloneDeriving #-}
module Types where

import Control.Monad.State
import qualified Data.Array as Array
import Data.List (intersperse)
import qualified Data.Map as Map

import Safe

import Language.Core.Core (Exp (..), Qual, Dcon, AnMname (..), Pname (..), Mname, Module (..), Id)
import Language.Core.Parser()

deriving instance Read Pname
deriving instance Read AnMname

newtype DbRef = DbRef Integer
instance Show DbRef where
  show (DbRef i) = show i
instance Read DbRef where
  readsPrec i s = map (\(v, r) -> (DbRef v, r)) $ readsPrec i s

type Arity = Int
data InterpExp = CoreExp Exp
               | RtExp ([HeapValue] -> HeapM Value) AnMname String Arity

rtExpArity (RtExp _ _ _ a) = a

instance Show InterpExp where
  show (CoreExp exp) = show exp
  show (RtExp _ _ name _) = "RtExp " ++ name

type HeapValue = Integer
data Value = DatabaseValue DbRef
           | Thunk { thunkEnv :: Env
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
showValue _ (IntegralValue n)     = "IntegralValue " ++ show n
showValue _ (CharValue c)         = "CharValue " ++ show c
showValue _ (StringValue s)       = "StringValue " ++ s
showValue _ (DatabaseValue (DbRef id)) = "DatabaseValue " ++ show id

showValueShort (Thunk env exp args)  = "Thunk " ++ show exp
showValueShort (DataValue tag args)  = "DataValue " ++ show tag
showValueShort (IntegralValue n)     = "IntegralValue " ++ show n
showValueShort (CharValue c)         = "CharValue " ++ show c
showValueShort (StringValue s)       = "StringValue " ++ s
showValueShort (DatabaseValue (DbRef id)) = "DatabaseValue " ++ show id



showArgs heap@(heapArray, _, _) args = indent 2 $ joinLines $ map f args
  where f arg = showValue heap $ fromJustNote ("showArgs: no arg " ++ show arg) 
                                              (heapArray Array.! arg)

showValueHeap (v, h) = showValue h v


addThunkArgs :: [HeapValue] -> Value -> HeapM Value
addThunkArgs [] t = return t
addThunkArgs a t@(Thunk {thunkArgs = a'}) = return $ t { thunkArgs = a' ++ a }
addThunkArgs a v = badArgs "addThunkArgs" =<< liftM (v:) (mapM heapGet a)



type HeapStore = Array.Array HeapValue (Maybe Value)
type Heap = (HeapStore, HeapValue, Maybe Env)
type HeapM = StateT Heap IO

runHeap f = runStateT f emptyHeap

heapGetFull :: HeapM Heap
heapGetFull = get

heapGetStore :: HeapM HeapStore
heapGetStore = get >>= (\(a, _, _) -> return a)

heapSetStore :: HeapStore -> HeapM ()
heapSetStore x = get >>= (\(_, b, c) -> put (x, b, c))

getDefaultEnv :: HeapM (Maybe Env)
getDefaultEnv = get >>= (\(_, _, c) -> return c)

setDefaultEnv :: Env -> HeapM ()
setDefaultEnv env = get >>= (\(a, b, _) -> put (a, b, Just env))

emptyHeap = (Array.listArray (0, 1024) (repeat Nothing), 0, Nothing)

heapAdd :: Value -> HeapM Integer
heapAdd v = do (vs, i, e) <- get
               let vs' = include vs i v
               put (vs', i + 1, e)
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
heapGet i = heapGetStore >>= return . fromJustNote ("heapGet: nothing at " ++ show i) .
                                           (Array.! i)
heapGet' (h,_,_) i = fromJustNote ("heapGet': nothing at " ++ show i)
                            (h Array.! i)
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
envMerge (Env m) (Env n) = Env $ Map.union m n

instance Show Env where
  show _ = "Env"

singleton :: a -> [a]
singleton = return

badArgs :: String -> [Value] -> HeapM a
badArgs n [] = error $ n ++ " not defined for []"
badArgs n args = heapGetFull >>= (\h -> error $ n ++ " not defined for "
                                            ++ concat (map (showValue h) args))

databaseValueMatcher (DatabaseValue _) = True
databaseValueMatcher _ = False


eval :: [Module] -> Env -> Id -> Id -> [HeapValue] -> HeapM HeapValue
eval mods env startMod startFun args = do
  heapAdd $ Thunk env (CoreExp varExp) args
  where mod = moduleById startMod mods
        varExp = Var (Just $ moduleMname mod , startFun)

      
moduleMname (Module mname _ _) = mname

moduleById id mods = headNote ("Couldn't find module with id " ++ id) $
                       [ mod | mod <- mods, moduleId mod == id ]
moduleId (Module (M (_, _, id)) _ _) = id
 
untilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
untilM t iter b
 | t b == False = do b' <- iter b
                     untilM t iter b'
 | otherwise    = return b
