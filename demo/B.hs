module B where

import Prelude hiding (head, length, filter, map)

type Word = String
type Definition = String
data Input = NewWord Word
           | NewDefinition Word Definition

type State = [(Word, [Definition])]

inputWord (NewWord w) = w
inputWord (NewDefinition w _) = w
inputDefinition (NewDefinition _ d) = d
isNewWord (NewWord _) = True
isNewWord _ = False
isNewDefinition (NewDefinition _ _) = True
isNewDefinition _ = False
isNewDefinitionFor w i = isNewDefinition i && w `strEq` inputWord i

map f [] = []
map f (x:xs) = (f x):(map f xs)

for xs f = map f xs

strEq :: String -> String -> Bool
strEq [] [] = True
strEq (a:as) (b:bs) = a == b && strEq as bs
strEq _ _ = False

filter f [] = []
filter f (x:xs) | f x       = x:(filter f xs)
                | otherwise = filter f xs

length :: [a] -> Integer -> Integer
length [] n = n
length (x:xs) n = length xs (n + 1)

initial_state :: [Input]
initial_state = [NewWord "Cat",
                 NewDefinition "Cat" "A regal housepet",
                 NewDefinition "Cat" "A tiny tiger",
                 NewWord "Dog",
                 NewDefinition "Dog" "A loyal housepet"]

app_state :: [Input] -> State
app_state inputs = for (map inputWord (filter isNewWord inputs)) (\w ->
                     (w, map inputDefinition
                           (filter (isNewDefinitionFor w) inputs)))

--head (x:_) = x
--length (x:xs) = 1 + length xs
--length [] = 0
--
--data TryInt =  TryIntP Int
--
--data TryList a = EmptyTryList | ConsTryList a (TryList a)
--
--data AppState = AppState { appStateWordsLength :: Integer
--                         , appStateWords :: [String]
--                         , appStateMostRecentWord :: String
--                         }
--
--thing True = "yes"
--thing False = "no"
--
--initial_state = ["welcome"]
--
--app_state words = AppState { appStateWordsLength = words_length words
--                           , appStateWords = words
--                           , appStateMostRecentWord = head words }
--
--words_length [] = 0
--words_length (w:ws) = (length w) + (words_length ws)
--
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
