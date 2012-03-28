module B where
import Inctime

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

