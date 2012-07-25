{-# LANGUAGE OverloadedStrings #-}
module Waltz where

import Control.Monad (forM_)

import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B

import Data.Enumerator (tryIO)
import qualified Data.Enumerator.List as EL

import Data.IORef
import Data.Maybe (fromJust, maybeToList)

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import Text.Blaze.Html5 hiding (head, map)
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8 (renderHtml)

-- TODOS
-- How do you make two different actionreceivers using two different columns of a html table?


import Debug.Trace

-- framework: page display


actionReceiverHtml actionReceiver causingAction children = H.form ! method "post" $ do
                                                             input ! type_ "hidden" ! name "actionReceiver" ! value (toValue $ actionReceiverName actionReceiver)
                                                             input ! type_ "hidden" ! name "actionSource" ! value (toValue $ show causingAction)
                                                             children

actionReceiverInput actionReceiverField Textfield = input ! name (toValue $ fieldName actionReceiverField)
actionReceiverInput actionReceiverField (Dropdown options) = select ! name (toValue $ fieldName actionReceiverField) $ do
                                                               forM_ options (\optionText -> option $ toHtml optionText)
actionReceiverInput actionReceiverField (Hidden v) = input ! type_ "hidden" ! name (toValue $ fieldName actionReceiverField) ! value (toValue v)

actionReceiverSend = input ! type_ "submit"

-- framework: user input

data Ensurer = EnsureIsOneOf [String] | EnsureIsNotEmpty | EnsureIsIntegral
  deriving (Show)

data ActionReceiverField = ActionReceiverField { fieldName :: String, validators :: [Ensurer] }
  deriving (Show)

data ActionReceiver t = ActionReceiver String [ActionReceiverField] ((ActionReceiverField -> String) -> Int -> t)
actionReceiverName (ActionReceiver name _ _) = name
actionReceiverBuilder (ActionReceiver _ _ builder) = builder

data ActionReceiverControl = Textfield | Dropdown [String] | Hidden String
  deriving (Show)

instance Show (ActionReceiver t) where
  show (ActionReceiver name fields func) = "ActionReceiver " ++ name ++ " " ++ (show fields)

-- http

app :: Read t => IORef [t] -> ([t] -> s) -> (s -> Maybe t -> Html) -> [ActionReceiver t] -> Application 
app db stateFunction representationFunction actionReceivers request = do
                    current_db <- tryIO $ readIORef db
                    request_body_chunks <- EL.consume
                    let request_body_query = parseQuery $ B.concat request_body_chunks
                    let action = processRequest actionReceivers request current_db request_body_query
                    let new_db = current_db ++ (maybeToList action)
                    let response = responseLBS
                                status200
                                [("Content-Type", B8.pack "text/html")]
                                (renderHtml $ representationFunction (stateFunction current_db) action)
                    tryIO $ writeIORef db new_db
                    return response

runApp sample_data stateFunction representationFunction actionReceivers = do
    putStrLn $ "http://localhost:8080/"
    db <- newIORef sample_data
    run 8080 $ app db stateFunction representationFunction actionReceivers 

processRequest actionReceivers request current_db request_body_query = case parseMethod $ requestMethod request of
                                                         (Right POST) ->  actionReceiverToAction actionReceivers request_body_query (length current_db)
                                                         otherwise -> Nothing

lastInQueryString :: Query -> String -> String
lastInQueryString query name = case filter (\(n, v) -> n == (B8.pack name)) $ query of
                                 []             -> error "nothing for " ++ name ++ " in " ++ (show query)
                                 lookup_matches -> B8.unpack $ fromJust $ snd $ last $ lookup_matches

actionReceiverWithName actionReceivers name = case filter (\h -> (actionReceiverName h) == name) actionReceivers of
                                []        -> error $ "no receiver with name " ++ name
                                receivers -> last receivers

actionReceiverToAction actionReceivers request_body_query n = let action_receiver_name = lastInQueryString request_body_query "actionReceiver"
                                                                  last_action = read $ lastInQueryString request_body_query "actionSource"
                                                                  action_receiver = actionReceiverWithName actionReceivers action_receiver_name
                                                                  field_getter action_receiver_field = lastInQueryString request_body_query $ fieldName action_receiver_field
                                                               in case validityErrors action_receiver field_getter of
                                                                    []     -> Just $ (actionReceiverBuilder action_receiver) field_getter n
                                                                    errors -> Just last_action -- include errors too, somehow

actionWith a receiver = a

validityErrors (ActionReceiver _ fields _) getter = errors fields
  where errors (field:fields) = fieldErrors field (validators field) $ errors fields
        errors [] = []
        fieldErrors field (v:vs) es = validate field v $ fieldErrors field vs es
        fieldErrors field [] es = es
        validate field (EnsureIsOneOf possibilities) errors = case filter ((==) $ getter field) possibilities of
                                                               []        -> (field,"not a valid possibility"):errors
                                                               otherwise -> errors
        validate field (EnsureIsNotEmpty) errors = case (getter field) == "" of
                                                     True  -> (field,"must not be empty"):errors
                                                     False -> errors
        validate field (EnsureIsIntegral) errors = case all (\c -> elem c "0123456789") $ getter field of
                                                     False -> (field,"must be integral"):errors
                                                     True  -> errors

wrap_in_html body = docTypeHtml $ do 
                      H.head $ do
                        H.title "Waltz App"
                      H.body body


