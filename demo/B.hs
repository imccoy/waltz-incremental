module B where
import Prelude hiding (map, filter, (++))
import Inctime
import InctimeHtml


filter = noIncLam (\f -> let filter' [] = []
                             filter' (x:xs)
                               | f x = x:(filter' xs)
                               | otherwise = filter' xs
                          in filter')

map = noIncLam (\f -> let map' [] = []
                          map' (x:xs) = (f x):(map' xs)
                       in map')
                       

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
isNewWordInput (NewWordInput _) = True
isNewWordInput _ = False
isNewDefinitionInput (NewDefinitionInput _ _) = True
isNewDefinitionInput _ = False
newWordInputs = filter `noIncApp` isNewWordInput
newDefinitionInputs = filter `noIncApp` isNewDefinitionInput
definitionInputsFor w = (filter `noIncApp` ((== w) . wordFrom)) . newDefinitionInputs
wordFrom (NewWordInput w) = w
wordFrom (NewDefinitionInput w _) = w
definitionFrom (NewDefinitionInput _ d) = d
definitionFrom _ = error "No definition"

wordDefinitions (WordDefinitions _ ds) = ds

app_state inputs
  = AppState { appStateNumWords = length $ words inputs
             , appStateNumDefinitions = elems_length $ map `noIncApp` wordDefinitions $ definitions inputs
             , appStateWords = words inputs
             , appStateDefinitions = definitions inputs
             }
  where words inputs = map `noIncApp` wordFrom $ newWordInputs inputs
        definitions inputs = map `noIncApp` (\w -> (WordDefinitions w $ word_definitions w inputs)) $ words inputs
        word_definitions w inputs = map `noIncApp` definitionFrom $ definitionInputsFor w inputs

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
  domElem "div" $ map (\(WordDefinitions word definitions) ->
                         domElem "div" [
                           tElem word,
                           domElem "div" $ map (\definition ->
                                                   domElem "div" [
                                                     tElem definition
                                                   ]
                                               ) definitions
                         ]
                      )
                      (appStateDefinitions state)
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
