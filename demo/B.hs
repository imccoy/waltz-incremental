module B where

import Prelude hiding (head, length)

head (x:_) = x
length (x:xs) = 1 + length xs
length [] = 0

data TryInt =  TryIntP Int

data TryList a = EmptyTryList | ConsTryList a (TryList a)

data AppState = AppState { appStateWordsLength :: Integer
                         , appStateWords :: [String]
                         , appStateMostRecentWord :: String
                         }

thing True = "yes"
thing False = "no"

initial_state = ["welcome"]

app_state words = AppState { appStateWordsLength = words_length words
                           , appStateWords = words
                           , appStateMostRecentWord = head words }

words_length [] = 0
words_length (w:ws) = (length w) + (words_length ws)

--page_view state = domElem "div" [
--  domElem "h1" [tElem "The Word Monster"],
--  domElem "p" [
--    tElem "I'm the word monster. Words are delicious! So far today, I've eaten ",
--    domElem "span" [
--      tElemB (IncBox show (appStateWordsLength state))
--    ],
--    tElem " letters."
--  ],
--  domElem "p" [
--    tElem "May I please have some more? I just had a marvellous '",
--    tElem (appStateMostRecentWord state),
--    tElem "'"
--  ],
--  elemA "form" [Attr "method" "post"] [
--    elemA "input" [Attr "name" "word"] [],
--    elemA "input" [Attr "type" "submit", Attr "value" "Feed the monster!"] []
--  ]
-- ]
