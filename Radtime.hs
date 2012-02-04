{-# LANGUAGE OverloadedStrings #-}
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

data OutputChange a = OutputChange String a
  deriving (Show)

applyOutputChange (OutputChange "base:GHCziNum.zp @ ghczmprim:GHCziTypes.Int base:GHCziNum.zdfNumInt" n) m = n + m


app parse_request state incrementalised_state_function representationFunction request = do
  request_body_chunks <- EL.consume
  let request_body_query = parseQuery $ B.concat request_body_chunks
  let maybe_input_change = processRequest parse_request request request_body_query
  current_state <- tryIO $ readIORef state
  let new_state = case maybe_input_change of
                    Nothing             -> current_state
                    (Just input_change) -> let output_change = incrementalised_state_function input_change
                                            in applyOutputChange output_change current_state
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


