module B where
import Inctime
import InctimeHtml

data TryInt =  TryIntP Int

data TryList a = EmptyTryList | ConsTryList a (TryList a)

data AppState = AppState { appStateWordsLength :: Int
                         , appStateWords :: [String]
                         , appStateMostRecentWord :: String
                         }

thing True = "yes"
thing False = "no"

app_state words = AppState { appStateWordsLength = words_length words
                           , appStateWords = words
                           , appStateMostRecentWord = head words }

words_length [] = 0
words_length (w:ws) = (length w) + (words_length ws)

-- if we call words_length_incrementalised directly from un-incrementalised
-- code, we run into trouble because we can't pass the typeclass argument. By
-- calling a specialised version, the machinery generates code to pick the
-- relevant typeclass argument.
words_length_stringy :: [String] -> Int
words_length_stringy = words_length


page_view state = domElem "div" [
  domElem "h1" [tElem "The Word Monster"],
  domElem "p" [
    tElem "I'm the word monster. Words are delicious! So far today, I've eaten ",
    domElem "span" [
      tElemB (IncBox show (appStateWordsLength state))
    ],
    tElem " letters."
  ],
  domElem "p" [
    tElem "May I please have some more? I just had a marvellous '",
    domElem "span" [
      tElem (appStateMostRecentWord state)
    ],
    tElem "'"
  ],
  elemA "form" [Attr "method" "post"] [
    elemA "input" [Attr "name" "word"] [],
    elemA "input" [Attr "type" "submit", Attr "value" "Feed the monster!"] []
  ]
 ]
