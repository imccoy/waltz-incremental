module B where

data TryList a = EmptyTryList | ConsTryList a (TryList a)

data AppState = AppState { appStateWordsLength :: Int, appStateWords :: [String], appStateMostRecentWord :: String }

app_state words = AppState { appStateWordsLength = words_length words
                           , appStateWords = words
                           , appStateMostRecentWord = head words }

words_length [] = 0
words_length (w:ws) = (length w) + (words_length ws)

