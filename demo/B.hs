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

-- if we call words_length_incrementalised directly from un-incrementalised
-- code, we run into trouble because we can't pass the typeclass argument. By
-- calling a specialised version, the machinery generates code to pick the
-- relevant typeclass argument.
words_length_stringy :: [String] -> Int
words_length_stringy = words_length
