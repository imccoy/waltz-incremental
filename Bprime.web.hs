{-# LANGUAGE OverloadedStrings #-}
import B
import Radtime


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
import Text.Blaze.Html5 hiding (head, map)
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8 (renderHtml)


app :: (IORef Int) -> (Int -> Html) -> Application
app state representationFunction request = do
  request_body_chunks <- EL.consume
  let request_body_query = parseQuery $ B.concat request_body_chunks
  let maybe_input_change = processRequest request request_body_query
  current_state <- tryIO $ readIORef state
  let new_state = case maybe_input_change of
                    Nothing             -> current_state
                    (Just input_change) -> let output_change = words_length_incrementalised input_change
                                            in applyOutputChange output_change current_state
  let response = responseLBS
              status200
              [("Content-Type", B8.pack "text/html")]
              (renderHtml $ representationFunction new_state)
  tryIO $ writeIORef state new_state
  return response

runApp = do
    putStrLn $ "http://localhost:8080/"
    state <- newIORef (0 :: Int)
    run 8080 $ app state page_view

processRequest request request_body_query = case parseMethod $ requestMethod request of
                                              (Right POST) -> Just $ parseRequest request_body_query
                                              otherwise -> Nothing

parseRequest query = let word = lastInQueryString query "word"
                      in InputChangewordszulengthZZC word
  
lastInQueryString :: Query -> String -> String
lastInQueryString query name = case filter (\(n, v) -> n == (B8.pack name)) $ query of
                                 []             -> error "nothing for " ++ name ++ " in " ++ (show query)
                                 lookup_matches -> B8.unpack $ fromJust $ snd $ last $ lookup_matches


main = runApp

page_view state = H.div $ do
  h1 "The Word Monster"
  H.p $ do
    toHtml $ "I'm the word monster. Words are delicious! So far today, I've eaten " ++ (show state) ++ " letters."
  H.p $ do
    toHtml $ ("May I please have some more?" :: String)
  H.form ! method "post" $ do
    input ! name "word"
    input ! type_ "submit" ! value "Feed the monster!"
