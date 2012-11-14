{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, MultiParamTypeClasses,
             ExistentialQuantification, UndecidableInstances,
             Rank2Types, FunctionalDependencies, FlexibleContexts,
             MagicHash, TypeFamilies, FlexibleInstances #-}
module Inctime where

import Control.Monad (MonadPlus, mzero)
import Data.Maybe (fromJust, maybeToList)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
import GHC.Prim

import Data.Data
import Unsafe.Coerce (unsafeCoerce)


class Incrementalised base incrementalised | incrementalised -> base where
  isIncrementalisedReplace :: incrementalised -> Bool
  isIncrementalisedIdentity :: incrementalised -> Bool
  mkIncrementalisedReplace :: base -> incrementalised
  mkIncrementalisedIdentity :: incrementalised
  extractReplaceValue :: incrementalised -> base

class ApplicableIncrementalised base incrementalised where 
  applyInputChange  :: incrementalised -> base -> base


data IncBox b = forall a. (Data a) => IncBox (a -> b) a

data IncBox_incrementalised b b_incrementalised
 = forall a a_incrementalised. 
     (ApplicableIncrementalised a a_incrementalised, Data a) =>
     IncBox_incrementalised (a -> b) a_incrementalised

instance ApplicableIncrementalised (IncBox b)
                                   (IncBox_incrementalised b b_incrementalised) where 
  applyInputChange (IncBox_incrementalised f v') (IncBox _ v)
    = IncBox f $ applyInputChange v' (unsafeCoerce v)

data IncrementalisedThing = forall base incrementalised.
                              (Incrementalised base incrementalised) =>
                              IncrementalisedThing incrementalised

isIncrementalisedIdentityThing (IncrementalisedThing a)
  = isIncrementalisedIdentity a

isIncrementalisedReplaceThing (IncrementalisedThing a)
  = isIncrementalisedReplace a

allIdentityOrReplace :: forall base incrementalised.
                        Incrementalised base incrementalised =>
                        [IncrementalisedThing] ->
                        base -> -- replacement case
                        incrementalised -> -- regular incrementalise case
                        incrementalised
allIdentityOrReplace incThings replace def
  | all isIncrementalisedIdentityThing incThings
  = mkIncrementalisedIdentity
  | all isIncrementalisedReplaceThing incThings
  = mkIncrementalisedReplace replace
  | otherwise
  = def



class Monad_incrementalised base incrementalised where
  bind_incrementalised :: incrementalised a -> (a -> incrementalised b) -> incrementalised b
  bind_discard_incrementalised :: incrementalised a -> incrementalised b -> incrementalised b
  return_incrementalised :: a -> incrementalised a
  fail :: String -> incrementalised a

class Monoid_incrementalised base incrementalised where
  mempty_incrementalised :: incrementalised
  mappend_incrementalised :: incrementalised -> incrementalised -> incrementalised
  mconcat_incrementalised :: [incrementalised] -> incrementalised

class (Eq base, Incrementalised base incrementalised) =>
      Eq_incrementalised base incrementalised | incrementalised -> base where
  eq_incrementalised_wrongcc :: base -> incrementalised -> base -> incrementalised -> Bool_incrementalised
  eq_incrementalised_wrongcc _ a _ b 
   | isIncrementalisedReplace a && isIncrementalisedReplace b
   = mkIncrementalisedReplace $ extractReplaceValue a == extractReplaceValue b
   | isIncrementalisedIdentity a && isIncrementalisedIdentity b
   = Bool_incrementalised_identity

eq_incrementalised :: (Incrementalised base incrementalised,
                        Eq base,
                        Eq_incrementalised base incrementalised) =>
                      base -> incrementalised ->
                      base -> incrementalised ->
                      Bool_incrementalised
eq_incrementalised = eq_incrementalised_wrongcc

typeclass_MonoidZLZR_incrementalised = undefined

typeclass_MonadZMZN_incrementalised = undefined

unit_incrementalised = ()

id_incrementalised = id

-- I'm not at all sure that the following work.

-- why are the foralls in this order? Because it seems to typecheck.
compose_incrementalised :: forall b. forall b_inc. (Incrementalised b b_inc) =>
                           forall c. forall c_inc. (Incrementalised c c_inc) =>
                           forall a. forall a_inc. (Incrementalised a a_inc) =>
                           (b -> c) -> (b_inc -> c_inc) ->
                           (a -> b) -> (a_inc -> b_inc) -> a_inc -> c_inc
compose_incrementalised f f_inc g g_inc = f_inc . g_inc

apply_incrementalised :: forall a a_inc b b_inc.
                         (Incrementalised a a_inc) =>
                         (Incrementalised b b_inc) =>
                         (a -> b) -> (a_inc -> b_inc) -> a_inc -> b_inc
apply_incrementalised f f_inc arg_inc = f_inc arg_inc


map_incrementalised :: forall a a_inc b b_inc.
                       (Incrementalised a a_inc) =>
                       (Incrementalised b b_inc) =>
                       (a -> b) -> (a -> a_inc -> b_inc) ->
                       [a] -> (BuiltinList_incrementalised a a_inc) ->
                       (BuiltinList_incrementalised b b_inc)
map_incrementalised f f_inc xs (BuiltinList_incrementalised_build_using_1 x) = 
  BuiltinList_incrementalised_build_using_1 (f x)
map_incrementalised f f_inc xs (BuiltinList_incrementalised)
  = BuiltinList_incrementalised -- empty list, do nothing
map_incrementalised f f_inc xs (BuiltinListCons_incrementalised h_change t_change)
  = case xs of -- we need to delay this until after matching
               -- BuiltinListCons_incrementalised because otherwise
               -- we pull on the xs list too soon. xs may be unavailable, in
               -- which case this causes catastrophic failure.
      h:t -> BuiltinListCons_incrementalised (f_inc h h_change)
                                             (map_incrementalised f f_inc t t_change)
map_incrementalised f f_inc xs (BuiltinList_incrementalised_identity)
  = BuiltinList_incrementalised_identity
{-
  = let xs' = map (\a -> f_inc a mkIncrementalisedIdentity) xs
     in case all isIncrementalisedIdentity xs' of
          True -> BuiltinList_incrementalised_identity
          False -> foldr BuiltinListCons_incrementalised
                         BuiltinList_incrementalised
                         xs'
-}
map_incrementalised f f_inc xs (BuiltinList_incrementalised_replace xs') = BuiltinList_incrementalised_replace $ map f xs'

filter_incrementalised :: forall a a_inc. (Incrementalised a a_inc) =>
                          (a -> Bool) -> (a -> a_inc -> Bool_incrementalised) ->
                          [a] -> (BuiltinList_incrementalised a a_inc) ->
                          (BuiltinList_incrementalised a a_inc)
filter_incrementalised f f_inc xs (BuiltinList_incrementalised_build_using_1 x)
  | f x = BuiltinList_incrementalised_build_using_1 x
  | otherwise = BuiltinList_incrementalised_identity
filter_incrementalised f f_inc xs (BuiltinList_incrementalised_identity)
  = BuiltinList_incrementalised_identity
filter_incrementalised f f_inc xs (BuiltinList_incrementalised_replace _) = error "filter_incrementalised replace"
filter_incrementalised f f_inc xs (BuiltinList_incrementalised_build_using_0 _) = error "filter_incrementalised build 0"
filter_incrementalised f f_inc xs (BuiltinList_incrementalised) = error "filter_incrementalised empty"
filter_incrementalised f f_inc xs (BuiltinListCons_incrementalised _ _) = error "filter_incrementalised cons"

class Num_incrementalised base incrementalised | incrementalised -> base where
  plus_incrementalised_wrongcc :: incrementalised -> incrementalised -> incrementalised

data Ordering_incrementalised = Ordering_incrementalised

class Ord_incrementalised base incrementalised | incrementalised -> base where
  compare_incrementalised :: base -> incrementalised -> base -> incrementalised -> Ordering_incrementalised

data Char_incrementalised = Char_incrementalised_C# Char#
                          | Char_incrementalised_replace Char
                          | Char_incrementalised_identity
  deriving Show

instance ApplicableIncrementalised Char Char_incrementalised where
  applyInputChange (Char_incrementalised_replace n) _ = n
  applyInputChange (Char_incrementalised_identity) m = m
  applyInputChange c _ = error $ "no applyInputChange for " ++ (show c)

instance Incrementalised Char Char_incrementalised where
  isIncrementalisedReplace (Char_incrementalised_replace _) = True
  isIncrementalisedReplace _                                = False
  isIncrementalisedIdentity Char_incrementalised_identity = True
  isIncrementalisedIdentity _                             = False
  extractReplaceValue (Char_incrementalised_replace v) = v
  extractReplaceValue _ = error "Not a replace"
  mkIncrementalisedIdentity = Char_incrementalised_identity
  mkIncrementalisedReplace = Char_incrementalised_replace

instance Eq_incrementalised Char Char_incrementalised where
  eq_incrementalised_wrongcc _ (Char_incrementalised_replace a) _ (Char_incrementalised_replace b)
    = case a == b of
        True -> True_incrementalised
        False -> False_incrementalised
  eq_incrementalised_wrongcc _ (Char_incrementalised_identity) _ (Char_incrementalised_identity)
    = Bool_incrementalised_identity
  eq_incrementalised_wrongcc _ _ _ _ = error "Difficult eq_incrementalised on Char"

instance Ord_incrementalised Char Char_incrementalised where
  compare_incrementalised _ _ = error "compare_incrementalised says don't go there."


data Bool_incrementalised = False_incrementalised
                          | True_incrementalised
                          | Bool_incrementalised_replace Bool
                          | Bool_incrementalised_identity
  deriving (Show)

mkIncrementalisedFalse = False_incrementalised
mkIncrementalisedTrue = True_incrementalised

instance Incrementalised Bool Bool_incrementalised where
  isIncrementalisedReplace (Bool_incrementalised_replace _) = True
  isIncrementalisedReplace _                                = False
  isIncrementalisedIdentity Bool_incrementalised_identity = True
  isIncrementalisedIdentity _                             = False
  extractReplaceValue (Bool_incrementalised_replace v) = v
  extractReplaceValue _ = error "Not a replace"
  mkIncrementalisedIdentity = Bool_incrementalised_identity
  mkIncrementalisedReplace = Bool_incrementalised_replace


data Int_incrementalised = Int_incrementalised_I# Int#
                         | Int_incrementalised_replace Int
                         | Int_incrementalised_identity
                         | Int_incrementalised_add Int_incrementalised Int_incrementalised
                         | Int_incrementalised_multiply Int_incrementalised Int_incrementalised
  deriving (Show)

instance Incrementalised Int Int_incrementalised where
  isIncrementalisedReplace (Int_incrementalised_replace _) = True
  isIncrementalisedReplace _                                = False
  isIncrementalisedIdentity Int_incrementalised_identity = True
  isIncrementalisedIdentity _                             = False
  extractReplaceValue (Int_incrementalised_replace v) = v
  extractReplaceValue _ = error "Not a replace"
  mkIncrementalisedIdentity = Int_incrementalised_identity
  mkIncrementalisedReplace = Int_incrementalised_replace


data Double_incrementalised = Double_incrementalised_I# Double#
                         | Double_incrementalised_replace Double
                         | Double_incrementalised_identity
                         | Double_incrementalised_add Double_incrementalised Double_incrementalised
                         | Double_incrementalised_multiply Double_incrementalised Double_incrementalised
  deriving (Show)

typeclass_NumDouble_incrementalised = undefined

instance Num_incrementalised Int Int_incrementalised where
  plus_incrementalised_wrongcc a b = Int_incrementalised_add a b

plus_incrementalised
  :: (Incrementalised base incrementalised
     ,Num base
     ,Num_incrementalised base incrementalised
     ) =>
     base -> incrementalised -> base -> incrementalised -> incrementalised
plus_incrementalised _ a _ b = plus_incrementalised_wrongcc a b

typeclass_ShowInt_incrementalised = undefined 
typeclass_ShowInteger_incrementalised = undefined
typeclass_ShowBool_incrementalised = undefined
typeclass_ShowFloat_incrementalised = undefined
typeclass_ShowDouble_incrementalised = undefined

show_incrementalised :: forall a. forall a_inc. 
                        (Incrementalised a a_inc, Show a_inc) => 
                        a_inc -> String_incrementalised
show_incrementalised _ = undefined

instance ApplicableIncrementalised Int Int_incrementalised where
  applyInputChange (Int_incrementalised_add a b) m = (applyInputChange a m) + (applyInputChange b m)
  applyInputChange (Int_incrementalised_replace n) _ = n
  applyInputChange (Int_incrementalised_identity) m = m
  applyInputChange c _ = error $ "no applyInputChange for " ++ (show c)

-- data ZMZN a = ZC a (ZMZN a) | []
data BuiltinList_incrementalised a a_incrementalised = 
                                                BuiltinList_incrementalised -- empty list constructor
                                              | BuiltinListCons_incrementalised
                                                   a_incrementalised 
                                                   (BuiltinList_incrementalised a a_incrementalised)
                                              | BuiltinList_incrementalised_build_using_1 a
                                              | BuiltinList_incrementalised_build_using_0 [a]
                                              | BuiltinList_incrementalised_identity -- that's ZMZN the type of lists, not ZMZN the empty list
                                              | BuiltinList_incrementalised_replace [a] -- replace the whole list with the specified value
  deriving (Show)

mkIncrementalisedBuiltinList = BuiltinList_incrementalised

mkIncrementalisedBuiltinListCons :: a ->
                                    a_incrementalised -> 
                                    [a] -> 
                                    BuiltinList_incrementalised a a_incrementalised ->
                                    BuiltinList_incrementalised a a_incrementalised
mkIncrementalisedBuiltinListCons _ h _ t = BuiltinListCons_incrementalised h t

instance (ApplicableIncrementalised elem elem_incrementalised) => 
            ApplicableIncrementalised ([elem]) (BuiltinList_incrementalised elem elem_incrementalised) where
  applyInputChange (BuiltinListCons_incrementalised hchange tchange) (h:t) = (applyInputChange hchange h):(applyInputChange tchange t)
  applyInputChange (BuiltinList_incrementalised) [] = []
  applyInputChange (BuiltinList_incrementalised_build_using_1 a) as = a:as
  applyInputChange (BuiltinList_incrementalised_build_using_0 as) a = a ++ as -- dubious, at best
  applyInputChange (BuiltinList_incrementalised_replace n) _ = n
  applyInputChange (BuiltinList_incrementalised_identity) m = m

instance (Incrementalised elem elem_incrementalised) => 
            Incrementalised ([elem]) (BuiltinList_incrementalised elem elem_incrementalised) where
  isIncrementalisedReplace (BuiltinList_incrementalised_replace _ ) = True
  isIncrementalisedReplace _                                        = False
  isIncrementalisedIdentity BuiltinList_incrementalised_identity = True
  isIncrementalisedIdentity _                                    = False
  extractReplaceValue (BuiltinList_incrementalised_replace v) = v
  extractReplaceValue _ = error "Not a replace"
  mkIncrementalisedIdentity = BuiltinList_incrementalised_identity
  mkIncrementalisedReplace e = BuiltinList_incrementalised_replace e

instance (Incrementalised elem elem_incrementalised, Eq elem) =>
            Eq_incrementalised [elem] (BuiltinList_incrementalised elem elem_incrementalised) where
  eq_incrementalised_wrongcc _ (BuiltinList_incrementalised_replace a) _ (BuiltinList_incrementalised_replace b)
    = case a == b of
        True -> True_incrementalised
        False -> False_incrementalised
  eq_incrementalised_wrongcc _ (BuiltinList_incrementalised_identity) _ (BuiltinList_incrementalised_identity)
    = Bool_incrementalised_identity
  eq_incrementalised_wrongcc _ _ _ _ = error "difficult eq_incrementalised on BuiltinList"

head_incrementalised :: forall base. forall incrementalised.
         Incrementalised [Char] (BuiltinList_incrementalised Char Char_incrementalised)
          => BuiltinList_incrementalised [Char] (BuiltinList_incrementalised Char Char_incrementalised)
          -> BuiltinList_incrementalised Char Char_incrementalised
head_incrementalised (BuiltinList_incrementalised_build_using_1 new_head)
  = BuiltinList_incrementalised_replace new_head

length_incrementalised :: forall a.
                          forall a_inc.
                          Incrementalised a a_inc => 
                          [a] ->
                          BuiltinList_incrementalised a a_inc -> Int_incrementalised
length_incrementalised _ (BuiltinList_incrementalised_replace a)
  = Int_incrementalised_replace $ length a
length_incrementalised _ (BuiltinList_incrementalised_build_using_1 _)
  = Int_incrementalised_add (Int_incrementalised_replace 1) (Int_incrementalised_identity)
length_incrementalised _ (BuiltinList_incrementalised_identity)
  = Int_incrementalised_identity
length_incrementalised ~(_:t) (BuiltinListCons_incrementalised h_change t_change)
  = length_incrementalised t t_change
length_incrementalised _ (BuiltinList_incrementalised )
  = error "length_incrementalised on []_incrementalised"
length_incrementalised _ _ = error "can't do incrementalised length"

type String_incrementalised = BuiltinList_incrementalised Char Char_incrementalised

instance Ord_incrementalised [v] (BuiltinList_incrementalised v v_inc) where
  compare_incrementalised _ _ = error "compare_incrementalised says don't go there."
-- the typeclass constraint here is to capture the typeclass dictionary value
-- within MapD, so the ApplicableIncrementalised typeclass instance doesn't need
-- to depend on it and the code generator can stay stupid.
data MapD k v = (Ord k) => MapD (Map k v) v
mapDempty d = MapD Map.empty d
mapDinsert k v (MapD m d) = MapD (Map.insert k v m) d
mapDlookup k (MapD m d) = case Map.lookup k m of
                           Just a  -> a
                           Nothing -> d
mapDalter f k (MapD m d) = MapD (Map.alter f' k m) d
  where f' Nothing  = Just $ f d
        f' (Just a) = Just $ f a
mapDmapWithKey f (MapD m d0) = MapD (Map.mapWithKey f m)
                                     (f (error "default has no key") d0)
mapDkeys (MapD m _) = Map.keys m

instance (Show k, Show v) => Show (MapD k v) where
  show (MapD m d) = show m

data MapD_incrementalised k k_inc v v_inc
   = MapD_incrementalised_atkey k v_inc
   | MapD_incrementalised_identity
   | MapD_incrementalised_replace (MapD k v)
  deriving (Show)

instance forall k k_inc v v_inc. 
         ((Incrementalised k k_inc), (Incrementalised v v_inc)) =>
         Incrementalised (MapD k v) (MapD_incrementalised k k_inc v v_inc) where
  isIncrementalisedReplace (MapD_incrementalised_replace _) = True
  isIncrementalisedReplace _ = False
  isIncrementalisedIdentity (MapD_incrementalised_identity) = True
  isIncrementalisedIdentity _ = False
  extractReplaceValue (MapD_incrementalised_replace a) = a
  extractReplaceValue _ = error "Not a replace"
  mkIncrementalisedIdentity = MapD_incrementalised_identity
  mkIncrementalisedReplace = MapD_incrementalised_replace

instance forall k k_inc v v_inc.
         ((ApplicableIncrementalised k k_inc), (ApplicableIncrementalised v v_inc)) => 
     ApplicableIncrementalised (MapD k v)
                               (MapD_incrementalised k k_inc
                                                     v v_inc) where
  applyInputChange (MapD_incrementalised_replace m) _ = m
  applyInputChange (MapD_incrementalised_identity) m = m
  applyInputChange (MapD_incrementalised_atkey k change) m
    = mapDinsert k v m
    where v = applyInputChange change $ mapDlookup k m

shuffle :: forall a b. (Ord b) => (a -> b) -> [a] -> MapD b [a]
shuffle f is = foldr (\i m -> mapDalter (i:) (f i) m) (mapDempty []) is

shuffle_incrementalised :: forall a a_inc b b_inc.
                           (Incrementalised a a_inc) =>
                           (Incrementalised b b_inc) =>
                           (Ord b, Ord_incrementalised b b_inc) =>
                           (a -> b) -> (a -> a_inc -> b_inc) ->
                           [a] -> (BuiltinList_incrementalised a a_inc) ->
                           (MapD_incrementalised b b_inc
                                    [a] (BuiltinList_incrementalised a a_inc))
shuffle_incrementalised f f_inc xs c@(BuiltinList_incrementalised_build_using_1 x)
  = MapD_incrementalised_atkey (f x) c
shuffle_incrementalised _ _ _ BuiltinList_incrementalised
  = error "shuffle_inc empty list change"
shuffle_incrementalised _ _ _ (BuiltinListCons_incrementalised _ _)
  = error "shuffle_inc cons change"
shuffle_incrementalised _ _ _ (BuiltinList_incrementalised_replace _)
  = error "shuffle_inc replace change"
shuffle_incrementalised _ _ _ (BuiltinList_incrementalised_identity)
  = error "shuffle_inc identity change"
shuffle_incrementalised _ _ _ (BuiltinList_incrementalised_build_using_0 _)
  = error "shuffle_inc build 0 change"

mapDmapWithKey_incrementalised :: forall k k_inc v1 v1_inc v2 v2_inc.
                                  (Incrementalised k k_inc,
                                    Incrementalised v1 v1_inc,
                                    Incrementalised v2 v2_inc) =>
                                  (k -> v1 -> v2) ->
                                  (k -> k_inc -> v1 -> v1_inc -> v2_inc) ->
                                  MapD k v1 ->
                                  MapD_incrementalised k k_inc v1 v1_inc ->
                                  MapD_incrementalised k k_inc v2 v2_inc
mapDmapWithKey_incrementalised f f_inc m (MapD_incrementalised_atkey k change)
  = let v0 = mapDlookup k m
        c1 = f_inc k mkIncrementalisedIdentity v0 change
     in MapD_incrementalised_atkey k c1
mapDmapWithKey_incrementalised _ _ _ (MapD_incrementalised_identity)
  = error "mapWithKey identity change"
mapDmapWithKey_incrementalised _ _ _ (MapD_incrementalised_replace _)
  = error "mapWithKey replace change"

mapDkeys_incrementalised :: forall k k_inc v v_inc.
                              (Incrementalised k k_inc,
                               Incrementalised v v_inc) =>
                              MapD k v ->
                              MapD_incrementalised k k_inc v v_inc ->
                              BuiltinList_incrementalised k k_inc
mapDkeys_incrementalised (MapD m _) (MapD_incrementalised_atkey k _)
  = case Map.lookup k m of
      Just a -> mkIncrementalisedIdentity
      Nothing -> BuiltinList_incrementalised_build_using_1 k

mapDlookup_incrementalised :: forall k k_inc v v_inc.
                              (Incrementalised k k_inc,
                               Incrementalised v v_inc) =>
                              k -> k_inc ->
                              MapD k v ->
                              MapD_incrementalised k k_inc v v_inc ->
                              v_inc
mapDlookup_incrementalised k k_inc (MapD _ _) (MapD_incrementalised_atkey k' c)
 | k == k'   = c
 | otherwise = mkIncrementalisedIdentity
 
