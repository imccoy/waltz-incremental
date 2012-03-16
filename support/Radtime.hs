{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, MultiParamTypeClasses, UndecidableInstances #-}
module Radtime where

import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import qualified Data.Enumerator.List as EL
import Data.Enumerator (tryIO)
import Data.Maybe (fromJust, maybeToList)
import Data.IORef
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Text.Blaze.Renderer.Utf8 (renderHtml)

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

app parse_request state incrementalised_state_function representationFunction request = do
  request_body_chunks <- EL.consume
  let request_body_query = parseQuery $ B.concat request_body_chunks
  let maybe_input_change = processRequest parse_request request request_body_query
  current_state <- tryIO $ readIORef state
  let new_state = case maybe_input_change of
                    Nothing             -> current_state
                    (Just input_change) -> let output_change = incrementalised_state_function input_change
                                            in applyInputChange output_change current_state
  let response = responseLBS
              status200
              [("Content-Type", B8.pack "text/html")]
              (renderHtml $ representationFunction new_state)
  tryIO $ writeIORef state new_state
  return response

runApp parse_request initial_state incrementalized_state_function page_view = do
    putStrLn $ "http://localhost:8080/"
    state <- newIORef initial_state
    run 8080 $ app parse_request state incrementalized_state_function page_view 

processRequest parse_request request request_body_query = 
  case parseMethod $ requestMethod request of
    (Right POST) -> Just $ parse_request request_body_query
    otherwise -> Nothing

lastInQueryString :: Query -> String -> String
lastInQueryString query name = case filter (\(n, v) -> n == (B8.pack name)) $ query of
                                 []             -> error "nothing for " ++ name ++ " in " ++ (show query)
                                 lookup_matches -> B8.unpack $ fromJust $ snd $ last $ lookup_matches

