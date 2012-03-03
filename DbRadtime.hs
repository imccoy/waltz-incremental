{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, MultiParamTypeClasses, UndecidableInstances #-}

module DbRadtime where

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

import Radtime (Char_incrementalised, Int_incrementalised, ZMZN_incrementalised (..), processRequest)

type DbAddress = String

class DbIncrementalised incrementalised where
  applyDbInputChange :: incrementalised -> DbAddress -> IO ()

appendAddress :: DbAddress -> String -> Int -> DbAddress
appendAddress address constructor fieldIndex = address ++ " " ++ newPiece
  where newPiece = constructor ++ "." ++ (show fieldIndex)

instance DbIncrementalised Char_incrementalised where
  applyDbInputChange change address = error "not implemented"

instance DbIncrementalised Int_incrementalised where
  applyDbInputChange change address = error "not implemented"

instance (DbIncrementalised elem_incrementalised) => 
            DbIncrementalised (ZMZN_incrementalised elem elem_incrementalised) where
  applyDbInputChange (ZMZN_incrementalised) address = error "not implemented"
  applyDbInputChange (ZC_incrementalised elem_incrementalised list_incrementalised) address = do
    applyDbInputChange elem_incrementalised (appendAddress address "ZC_incrementalised" 0)
  applyDbInputChange change address = error "not implemented"


app parse_request state incrementalised_state_function representationFunction request = do
  request_body_chunks <- EL.consume
  let request_body_query = parseQuery $ B.concat request_body_chunks
  let maybe_input_change = processRequest parse_request request request_body_query
  case maybe_input_change of
    Nothing             -> return ()
    (Just input_change) -> let output_change = incrementalised_state_function input_change
                            in tryIO $ applyDbInputChange output_change ""
  let response = responseLBS
              status200
              [("Content-Type", B8.pack "text/html")]
              (renderHtml $ representationFunction state)
  return response

runAppDb parse_request initial_state incrementalized_state_function page_view = do
    putStrLn $ "http://localhost:8080/"
    run 8080 $ app parse_request initial_state incrementalized_state_function page_view 


