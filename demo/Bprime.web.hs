{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
import B
import Radtime
import Binstances

import Text.Blaze.Html5 hiding (head, map)
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


parse_request query = let word = lastInQueryString query "word"
                      in ZC_incrementalised_build_using_1 word
  
main = runApp parse_request (app_state ["hello"]) appzustate_incrementalised page_view 

page_view state = H.div $ do
  h1 "The Word Monster"
  H.p $ do
    toHtml $ "I'm the word monster. Words are delicious! So far today, I've eaten " ++ (show $ appStateWordsLength state) ++ " letters."
  H.p $ do
    toHtml $ ("May I please have some more? I just had a marvellous '" ++ (appStateMostRecentWord state) ++ "'")
  H.form ! method "post" $ do
    input ! name "word"
    input ! type_ "submit" ! value "Feed the monster!"
