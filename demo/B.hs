module B where
import Prelude hiding (map, filter, (++))
import Inctime
import InctimeHtml


filter f []                 = []
filter f (x:xs) | f x       = x:(filter f xs)
                | otherwise = filter f xs

map f [] = []
map f (x:xs) = (f x):(map f xs)
                       
[] ++ a = a
(x:xs) ++ a = x:(xs ++ a)



data WordDefinitions = WordDefinitions String [String]

data AppState = AppState { appStateNumWords :: Int
                         , appStateNumDefinitions :: Int
                         , appStateWords :: [String]
                         , appStateDefinitions :: [WordDefinitions]
                         }

data Input = NewWordInput String
           | NewDefinitionInput String String
isNewWordInput i = case i of
                     (NewWordInput _) -> True
                     otherwise        -> False
isNewDefinitionInput i = case i of
                           (NewDefinitionInput _ _) -> True
                           otherwise                -> False
newWordInputs = filter isNewWordInput
newDefinitionInputs = filter isNewDefinitionInput
definitionInputsFor w inputs = filter (inputContainsWord w) (newDefinitionInputs inputs)
wordFrom (NewWordInput w) = w
wordFrom (NewDefinitionInput w _) = w
inputContainsWord word i = wordFrom i == word
definitionFrom i = case i of 
                     (NewDefinitionInput _ d) -> d
                     otherwise                -> error "No definition"

wordDefinitions (WordDefinitions _ ds) = ds

app_state inputs
  = AppState { appStateNumWords = length (words inputs)
             , appStateNumDefinitions = elems_length (map wordDefinitions (definitions inputs))
             , appStateWords = words inputs
             , appStateDefinitions = definitions inputs
             }
  where words inputs = map wordFrom (newWordInputs inputs)
        definitions inputs = let f inputs w = word_definitions w inputs
                              in map (f inputs) (words inputs)
        word_definitions w inputs = WordDefinitions w (map definitionFrom (definitionInputsFor w inputs))

elems_length [] = 0
elems_length (w:ws) = (length w) + (elems_length ws)

-- if we call words_length_incrementalised directly from un-incrementalised
-- code, we run into trouble because we can't pass the typeclass argument. By
-- calling a specialised version, the machinery generates code to pick the
-- relevant typeclass argument.
words_length_stringy :: [String] -> Int
words_length_stringy = elems_length


page_view state = domElem "div" [
  domElem "h1" [tElem "The Urbane Dictionary"],
  domElem "p" [
    tElem ("I'm the urbane dictionary. The definitions are classy, even when" ++
           "the words are not."),
    tElemB (IncBox (\x -> show x ++ " words.") (appStateNumWords state)),
    tElemB (IncBox (\x -> show x ++ " definitions.") (appStateNumDefinitions state))
  ],
  domElem "div" (map (\(WordDefinitions word definitions) ->
                        domElem "div" [
                          tElem word,
                          domElem "div" (map (\definition ->
                                                 domElem "div" [
                                                   tElem definition
                                                 ]
                                             ) definitions)
                        ]
                     )
                     (appStateDefinitions state))
  ,
  elemA "form" [Attr "method" "post"] [
    elemA "input" [Attr "name" "word"] [],
    elemA "input" [Attr "name" "definition"] [],
    elemA "input" [Attr "name" "action",
                   Attr "type" "submit",
                   Attr "value" "Add Definition"] [],
    elemA "input" [Attr "name" "action", 
                   Attr "type" "submit",
                   Attr "value" "Add Word"] []
  ]
 ]
