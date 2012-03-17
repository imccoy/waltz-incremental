{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, MultiParamTypeClasses, UndecidableInstances #-}
module Radtime where

import Control.Exception
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Enumerator.List as EL
import Data.Enumerator (tryIO)
import Data.IORef
import Data.List
import Data.Maybe (fromJust, maybeToList)
import qualified Data.Text as T
import Debug.Trace
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

class Incrementalised incrementalised base where 
  applyInputChange  :: incrementalised -> base -> base

data Char_incrementalised = Char_incrementalised_hoist
                          | Char_incrementalised_replace Char
                          | Char_incrementalised_identity
  deriving Show

instance Incrementalised Char_incrementalised Char where
  applyInputChange (Char_incrementalised_replace n) _ = n
  applyInputChange (Char_incrementalised_identity) m = m
  applyInputChange c _ = error $ "no applyInputChange for " ++ (show c)

data Int_incrementalised = Int_incrementalised_add Int_incrementalised Int_incrementalised
                         | Int_incrementalised_multiply Int_incrementalised Int_incrementalised
                         | Int_incrementalised_identity
                         | Int_incrementalised_replace Int
                         | Int_incrementalised_hoist
  deriving (Show)

zdfNumInt_incrementalised = undefined -- will be passed to zp_incrementalised, which will ignore it

zp_incrementalised _ a b = Int_incrementalised_add a b
zp_incrementalised _ a b = Int_incrementalised_add a b

zdfShowInt_incrementalised = undefined -- will be passed to show_incrementalised, which will ignore it

show_incrementalised _ (Int_incrementalised_replace a) = ZMZN_incrementalised_replace $ show a


zd = ($)
zd_incrementalised = ($)

instance Incrementalised Int_incrementalised Int where
  applyInputChange (Int_incrementalised_add a b) m = (applyInputChange a m) + (applyInputChange b m)
  applyInputChange (Int_incrementalised_replace n) _ = n
  applyInputChange (Int_incrementalised_identity) m = m
  applyInputChange c _ = error $ "no applyInputChange for " ++ (show c)

-- data ZMZN a = ZC a (ZMZN a) | []
data ZMZN_incrementalised a a_incrementalised = ZC_incrementalised
                                                   a_incrementalised 
                                                   (ZMZN_incrementalised a a_incrementalised)
                                              | ZC_incrementalised_build_using_1 a
                                              | ZC_incrementalised_build_using_0 [a]
                                              | ZMZN_incrementalised_identity -- that's ZMZN the type of lists, not ZMZN the empty list
                                              | ZMZN_incrementalised_replace [a] -- replace the whole list with the specified value
                                              | ZMZN_incrementalised_hoist
                                              | ZMZN_incrementalised -- empty list constructor

instance (Incrementalised elem_incrementalised elem) => 
            Incrementalised (ZMZN_incrementalised elem elem_incrementalised) ([elem]) where
  applyInputChange (ZC_incrementalised hchange tchange) (h:t) = (applyInputChange hchange h):(applyInputChange tchange t)
  applyInputChange (ZC_incrementalised_build_using_1 a) as = a:as
  applyInputChange (ZC_incrementalised_build_using_0 as) a = a ++ as -- dubious, at best
  applyInputChange (ZMZN_incrementalised_replace n) _ = n
  applyInputChange (ZMZN_incrementalised_identity) m = m

head_incrementalised (ZC_incrementalised_build_using_1 new_head) = ZMZN_incrementalised_replace new_head
head_incrementalised _ = error "can't do incrementalised head"
--head_incrementalised (ZC_incrementalised_build_using_1 new_head :: ZMZN_incrementalised [a] (ZMZN_incrementalised a a_incrementalised)) = ZMZN_incrementalised_replace new_head :: a_incrementalised
--head_incrementalised _ = error "can't do incrementalised head"

length_incrementalised (ZMZN_incrementalised_replace a) = Int_incrementalised_replace $ length a
length_incrementalised _ = error "can't do incrementalised length"

zpzp_incrementalised (ZMZN_incrementalised_replace a) (ZMZN_incrementalised_replace b)
 = ZMZN_incrementalised_replace $ a ++ b
zpzp_incrementalised _ _ = error "can't do incrementalised concat"

