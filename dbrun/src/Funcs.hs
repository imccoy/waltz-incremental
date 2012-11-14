{-# Language MagicHash #-}
module Funcs where


import FuncsAux
import Prelude (Bool (True, False),
                Char,
                fromEnum,
                Int,
                Maybe (..),
                otherwise,
                Ordering (LT, GT, EQ),
                (&&), (||)
                )

listLength :: [a] -> Int
listLength [] = 0
listLength (x:xs) = 1 `intPlus` listLength xs

listMap :: (a -> b) -> [a] -> [b]
listMap f [] = []
listMap f (a:as) = (f a):(listMap f as)

listFilter :: (a -> Bool) -> [a] -> [a]
listFilter _ [] = []
listFilter f (a:as)
 | f a       = a:(listFilter f as)
 | otherwise = listFilter f as

listAll :: (a -> Bool) -> [a] -> Bool
listAll f (x:xs) = case f x of
                     True -> listAll f xs
                     False -> False
listAll f [] = True

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f b [] = b
foldr f b (a:as) = f a (foldr f b as)

class Eql a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  -- Minimal complete definition: either == or /=.
  a == b = case a /= b of
             True -> False
             False -> True
  a /= b = case a == b of
             True -> False
             False -> True
  

class (Eql a) => Ordr a where
  compare              :: a -> a -> Ordering
  (<), (<=), (>=), (>) :: a -> a -> Bool
  max, min             :: a -> a -> a

      -- Minimal complete definition:
      --      (<=) or compare
      -- Using compare can be more efficient for complex types.
  compare x y
       | x == y    =  EQ
       | x <= y    =  LT
       | otherwise =  GT

  x <= y           =  compare x y /= GT
  x <  y           =  compare x y == LT
  x >= y           =  compare x y /= LT
  x >  y           =  compare x y == GT

  -- note that (min x y, max x y) = (x,y) or (y,x)
  max x y 
       | x >= y    =  x
       | otherwise =  y
  min x y
       | x <  y    =  x
       | otherwise =  y 

instance Eql Ordering where
  GT == GT = True
  EQ == EQ = True
  LT == LT = True
  _  == _  = False

instance Eql Int where
  a == b = intEq a b

instance Eql Char where
  a == b = fromEnum a == fromEnum b

instance Eql a => Eql [a] where
  (x:xs) == (y:ys) = x == y && xs == ys
  []     == [] = True
  _      == _   = False

instance Ordr a => Ordr [a] where
  (x:xs) <= (y:ys) = x <= y && (x /= y || xs <= ys)
  []     <= []     = True -- equal
  []     <= (_:_)  = True
  (_:_)  <= []     = False

data Map k v = MapEmpty
             | MapTree k v (Map k v) (Map k v)

a $ b = a b

mapEmpty = MapEmpty

mapInsert k v MapEmpty = MapTree k v MapEmpty MapEmpty
mapInsert k v (MapTree k' v' l r)
  | k < k'  = MapTree k' v' (mapInsert k v l) r
  | k == k' = MapTree k' v l r
  | k > k'  = MapTree k' v' l (mapInsert k v r)

mapGet :: Ordr k => k -> Map k v -> Maybe v
mapGet k MapEmpty = Nothing
mapGet k (MapTree k' v l r)
  | k < k'  = mapGet k l
  | k == k' = Just v
  | k > k'  = mapGet k r

mapDelete _ MapEmpty = MapEmpty
mapDelete k (MapTree k' v l r)
  | k < k'  = MapTree k' v (mapDelete k l) r
  | k == k' = mapMerge l r
  | k > k'  = MapTree k' v l (mapDelete k r)

mapMerge MapEmpty r = r
mapMerge l MapEmpty = l
mapMerge (MapTree k v ll lr) r = MapTree k v (mapMerge ll lr) r

mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
mapWithKey f MapEmpty = MapEmpty
mapWithKey f (MapTree k v l r) = MapTree k
                                        (f k v)
                                        (mapWithKey f l)
                                        (mapWithKey f r)

mapAlter :: Ordr k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
mapAlter f k m = case f (mapGet k m) of
                   Nothing -> mapDelete k m
                   Just v  -> mapInsert k v m
