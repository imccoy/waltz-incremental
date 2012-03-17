module B where
import InctimeHtmlPrime

data TryList a = EmptyTryList | ConsTryList a (TryList a)

data AppState = AppState { appStateWordsLength :: Int, appStateWords :: [String], appStateMostRecentWord :: String }

app_state words = AppState { appStateWordsLength = words_length words
                           , appStateWords = words
                           , appStateMostRecentWord = head words }

words_length [] = 0
words_length (w:ws) = (length w) + (words_length ws)

page_view state = domElem "div" [
  domElem "h1" [tElem "The Word Monster"],
  domElem "p" [
    tElem $ "I'm the word monster. Words are delicious! So far today, I've eaten ",
    domElem "span" [
      tElem $ show $ appStateWordsLength state
    ],
    tElem $ " letters."
  ],
  domElem "p" [
    tElem $ "May I please have some more? I just had a marvellous '",
    domElem "span" [
      tElem $ appStateMostRecentWord state
    ],
    tElem $ "'"
  ],
  elemA "form" [Attr "method" "post"] [
    elemA "input" [Attr "name" "word"] [],
    elemA "input" [Attr "type" "submit", Attr "value" "Feed the monster!"] []
  ]
 ]
