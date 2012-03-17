{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module InctimeWeb where

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

import InctimeHtmlPrime
import InctimeUtils
import Radtime

wrapDom :: Dom -> Dom
wrapDom d = domElem "html" 
               [domElem "head"
                 [elemA "script" [Attr "src" "rts-common.js"] []
                 ,elemA "script" [Attr "src" "rts-plain.js"] []
                 ,elemA "script" [Attr "src" "http://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js"] []
                 ,elemA "script" [Attr "src" "Bprime.js"] []
                 ,elemA "script" [Attr "src" "Bprime.web.js"] []
                 ,elemA "script" [Attr "src" "Inctime.js"] []
                 ,elemA "script" [Attr "src" "InctimeUtils.js"] []
                 ,elemA "script" [Attr "src" "InctimeHtmlPrime.js"] []
                 ]
               ,domElem "body"
                 [d]
               ]

respond404 = return $ responseLBS status404
                         [("Content-Type", B8.pack "text/plain")]
                         (LB8.pack "404 error")


app parse_request state incrementalised_state_function representationFunction request
  | (length $ pathInfo request) > 0 &&
    ".js" `isSuffixOf` (T.unpack $ last $ pathInfo request) = do
  liftIO $ handle (\(e :: IOException) -> putStrLn (show e) >> respond404) $ do
    content <- readFile $ (concat $ intersperse "/" $ map T.unpack $ pathInfo request)
    return $ responseLBS status200
                         [("Content-Type", B8.pack "script/javascript")]
                         (LB8.pack content)

  | otherwise = do
  request_body_chunks <- (requestBody request) $$ CL.consume
  let request_body_query = stringifyQuery $ parseQuery $ B.concat request_body_chunks
  let maybe_input_change = processRequest parse_request request request_body_query
  current_state <- liftIO $ readIORef state
  let new_state = case maybe_input_change of
                    Nothing             -> current_state
                    (Just input_change) -> let output_change = incrementalised_state_function input_change
                                            in applyInputChange output_change current_state
  let response = responseLBS
              status200
              [("Content-Type", B8.pack "text/html")]
              (LB8.pack $ renderHtml $ wrapDom $ representationFunction new_state)
  liftIO $ writeIORef state new_state
  return response

runApp parse_request initial_state incrementalized_state_function page_view = do
    putStrLn $ "http://localhost:8080/"
    state <- newIORef initial_state
    run 8080 $ app parse_request state incrementalized_state_function page_view 

processRequest parse_request request request_body_query = 
  case parseMethod $ requestMethod request of
    (Right POST) -> Just $ parse_request request_body_query
    otherwise -> Nothing

stringifyQuery :: Query -> [(String, Maybe String)]
stringifyQuery = map (\(a, b) -> (B8.unpack a, fmap B8.unpack b))

