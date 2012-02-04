{-# LANGUAGE OverloadedStrings #-}
import B
import Radtime

import Text.Blaze.Html5 hiding (head, map)
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


parse_request query = let word = lastInQueryString query "word"
                      in InputChangewordszulengthZZC word
  
main = runApp parse_request (0 :: Int) words_length_incrementalised page_view 

page_view state = H.div $ do
  h1 "The Word Monster"
  H.p $ do
    toHtml $ "I'm the word monster. Words are delicious! So far today, I've eaten " ++ (show state) ++ " letters."
  H.p $ do
    toHtml $ ("May I please have some more?" :: String)
  H.form ! method "post" $ do
    input ! name "word"
    input ! type_ "submit" ! value "Feed the monster!"
