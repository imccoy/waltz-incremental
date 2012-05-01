{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, MultiParamTypeClasses,
             ExistentialQuantification, UndecidableInstances,
             Rank2Types, FunctionalDependencies, FlexibleContexts,
             MagicHash #-}
module Inctime where

import Data.Maybe (fromJust, maybeToList)
import Debug.Trace
import GHC.Prim

data Obj = forall a. Obj a
 
class Incrementalised base incrementalised | incrementalised -> base where
  isIncrementalisedReplace :: incrementalised -> Bool
  isIncrementalisedHoist :: incrementalised -> Bool
  isIncrementalisedIdentity :: incrementalised -> Bool
  mkIncrementalisedReplace :: base -> incrementalised
  mkIncrementalisedIdentity :: incrementalised
  mkIncrementalisedHoist :: incrementalised
  extractReplaceValue :: incrementalised -> base

class ApplicableIncrementalised base incrementalised where 
  applyInputChange  :: incrementalised -> base -> base


class Monad_incrementalised base incrementalised where
  bind_incrementalised :: incrementalised a -> (a -> incrementalised b) -> incrementalised b
  bind_discard_incrementalised :: incrementalised a -> incrementalised b -> incrementalised b
  return_incrementalised :: a -> incrementalised a
  fail :: String -> incrementalised a

class Monoid_incrementalised base incrementalised where
  mempty_incrementalised :: incrementalised
  mappend_incrementalised :: incrementalised -> incrementalised -> incrementalised
  mconcat_incrementalised :: [incrementalised] -> incrementalised

typeclass_MonoidZLZR_incrementalised = undefined

typeclass_MonadZMZN_incrementalised = undefined

unit_incrementalised = ()

id_incrementalised = id

compose_incrementalised :: forall a. forall a_inc. 
                           forall b. forall b_inc.
                           forall c. forall c_inc.
                           (b_inc -> c_inc) -> (a_inc -> b_inc) -> a_inc -> c_inc
compose_incrementalised = (.)




class Num_incrementalised base incrementalised | incrementalised -> base where
  plus_incrementalised_wrongcc :: incrementalised -> incrementalised -> incrementalised


data Char_incrementalised = Char_incrementalised_C# Char#
                          | Char_incrementalised_replace Char
                          | Char_incrementalised_identity
                          | Char_incrementalised_hoist
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
  isIncrementalisedHoist Char_incrementalised_hoist = True
  isIncrementalisedHoist _                          = False
  extractReplaceValue (Char_incrementalised_replace v) = v
  extractReplaceValue _ = error "Not a replace"
  mkIncrementalisedIdentity = Char_incrementalised_identity
  mkIncrementalisedReplace = Char_incrementalised_replace
  mkIncrementalisedHoist = Char_incrementalised_hoist

data Bool_incrementalised = Bool_incrementalised_False
                          | Bool_incrementalised_True
                          | Bool_incrementalised_replace Bool
                          | Bool_incrementalised_identity
                          | Bool_incrementalised_hoist
  deriving (Show)

instance Incrementalised Bool Bool_incrementalised where
  isIncrementalisedReplace (Bool_incrementalised_replace _) = True
  isIncrementalisedReplace _                                = False
  isIncrementalisedIdentity Bool_incrementalised_identity = True
  isIncrementalisedIdentity _                             = False
  isIncrementalisedHoist Bool_incrementalised_hoist = True
  isIncrementalisedHoist _                          = False
  extractReplaceValue (Bool_incrementalised_replace v) = v
  extractReplaceValue _ = error "Not a replace"
  mkIncrementalisedIdentity = Bool_incrementalised_identity
  mkIncrementalisedReplace = Bool_incrementalised_replace
  mkIncrementalisedHoist = Bool_incrementalised_hoist


data Int_incrementalised = Int_incrementalised_I# Int#
                         | Int_incrementalised_replace Int
                         | Int_incrementalised_hoist
                         | Int_incrementalised_identity
                         | Int_incrementalised_add Int_incrementalised Int_incrementalised
                         | Int_incrementalised_multiply Int_incrementalised Int_incrementalised
  deriving (Show)

instance Incrementalised Int Int_incrementalised where
  isIncrementalisedReplace (Int_incrementalised_replace _) = True
  isIncrementalisedReplace _                                = False
  isIncrementalisedIdentity Int_incrementalised_identity = True
  isIncrementalisedIdentity _                             = False
  isIncrementalisedHoist Int_incrementalised_hoist = True
  isIncrementalisedHoist _                          = False
  extractReplaceValue (Int_incrementalised_replace v) = v
  extractReplaceValue _ = error "Not a replace"
  mkIncrementalisedIdentity = Int_incrementalised_identity
  mkIncrementalisedReplace = Int_incrementalised_replace
  mkIncrementalisedHoist = Int_incrementalised_hoist


data Double_incrementalised = Double_incrementalised_I# Double#
                         | Double_incrementalised_replace Double
                         | Double_incrementalised_hoist
                         | Double_incrementalised_identity
                         | Double_incrementalised_add Double_incrementalised Double_incrementalised
                         | Double_incrementalised_multiply Double_incrementalised Double_incrementalised
  deriving (Show)

typeclass_NumDouble_incrementalised = undefined

instance Num_incrementalised Int Int_incrementalised where
  plus_incrementalised_wrongcc a b = Int_incrementalised_add a b

plus_incrementalised
  :: (Incrementalised base incrementalised
     ,Num_incrementalised base incrementalised
     ) =>
     incrementalised -> incrementalised -> incrementalised
plus_incrementalised = plus_incrementalised_wrongcc

typeclass_ShowInt_incrementalised = undefined 
typeclass_ShowInteger_incrementalised = undefined
typeclass_ShowBool_incrementalised = undefined
typeclass_ShowFloat_incrementalised = undefined
typeclass_ShowDouble_incrementalised = undefined

show_incrementalised :: forall a. forall a_inc. ((forall tc. tc) -> a_inc -> String_incrementalised)
show_incrementalised _ _ = undefined

instance ApplicableIncrementalised Int Int_incrementalised where
  applyInputChange (Int_incrementalised_add a b) m = (applyInputChange a m) + (applyInputChange b m)
  applyInputChange (Int_incrementalised_replace n) _ = n
  applyInputChange (Int_incrementalised_identity) m = m
  applyInputChange c _ = error $ "no applyInputChange for " ++ (show c)

-- data ZMZN a = ZC a (ZMZN a) | []
data BuiltinList_incrementalised a a_incrementalised = ZMZN_incrementalised -- empty list constructor
                                              | BuiltinList_incrementalised
                                                   a_incrementalised 
                                                   (BuiltinList_incrementalised a a_incrementalised)
                                              | BuiltinList_incrementalised_replace [a] -- replace the whole list with the specified value
                                              | BuiltinList_incrementalised_hoist
                                              | BuiltinList_incrementalised_identity -- that's ZMZN the type of lists, not ZMZN the empty list
                                              | BuiltinList_incrementalised_build_using_1 a
                                              | BuiltinList_incrementalised_build_using_0 [a]
  deriving (Show)

instance (ApplicableIncrementalised elem elem_incrementalised) => 
            ApplicableIncrementalised ([elem]) (BuiltinList_incrementalised elem elem_incrementalised) where
  applyInputChange (BuiltinList_incrementalised hchange tchange) (h:t) = (applyInputChange hchange h):(applyInputChange tchange t)
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
  isIncrementalisedHoist BuiltinList_incrementalised_hoist = True
  isIncrementalisedHoist _                                 = False
  extractReplaceValue (BuiltinList_incrementalised_replace v) = v
  extractReplaceValue _ = error "Not a replace"
  mkIncrementalisedIdentity = BuiltinList_incrementalised_identity
  mkIncrementalisedReplace e = BuiltinList_incrementalised_replace e
  mkIncrementalisedHoist = BuiltinList_incrementalised_hoist

head_incrementalised :: forall base. forall incrementalised.
         Incrementalised [Char] (BuiltinList_incrementalised Char Char_incrementalised)
          => BuiltinList_incrementalised [Char] (BuiltinList_incrementalised Char Char_incrementalised)
          -> BuiltinList_incrementalised Char Char_incrementalised
head_incrementalised (BuiltinList_incrementalised_build_using_1 new_head)
  = BuiltinList_incrementalised_replace new_head

length_incrementalised :: forall a.
                          forall a_inc.
                          Incrementalised a a_inc => 
                          BuiltinList_incrementalised a a_inc -> Int_incrementalised
length_incrementalised (BuiltinList_incrementalised_replace a)
  = Int_incrementalised_replace $ length a
length_incrementalised (BuiltinList_incrementalised_build_using_1 _)
  = Int_incrementalised_add (Int_incrementalised_replace 1) (Int_incrementalised_identity)
length_incrementalised (BuiltinList_incrementalised_identity)
  = Int_incrementalised_identity
length_incrementalised (BuiltinList_incrementalised h_change t_change)
  = length_incrementalised t_change
length_incrementalised (BuiltinList_incrementalised_hoist)
  = Int_incrementalised_identity
length_incrementalised _ = error "can't do incrementalised length"

type String_incrementalised = BuiltinList_incrementalised Char Char_incrementalised






