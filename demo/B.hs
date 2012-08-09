module B where
import Prelude
import Data.Map (Map)
import qualified Data.Map as Map
import Inctime
import InctimeHtml


[] `append` a = a
(x:xs) `append` a = x:(xs `append` a)



data WordDefinitions = WordDefinitions String [String]

data AppState = AppState { appStateNumWords :: Int
                         , appStateNumDefinitions :: Int
                         , appStateWords :: [String]
                         , appStateDefinitions :: MapD String [String]
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
                     otherwise                -> "NOTADEFINITION" -- should be error, strictly speaking

definitionsFrom (WordDefinitions _ ds) = ds

wordsFromInputs inputs = map wordFrom (newWordInputs inputs)
definitionsFromInputsFor w inputs = map definitionFrom (definitionInputsFor w inputs)
definitions inputs = mapDmapWithKey definitionsFromInputsFor (shuffle wordFrom inputs)

app_state inputs
  = AppState { appStateNumWords = length (wordsFromInputs inputs)
             , appStateNumDefinitions = length (newDefinitionInputs inputs)
             , appStateWords = wordsFromInputs inputs
             , appStateDefinitions = definitions inputs
             }

page_view state = domElem "div" [
  domElem "h1" [tElem "The Urbane Dictionary"],
  domElem "p" [
    tElem ("I'm the urbane dictionary. The definitions are classy, even when " `append`
           "the words are not."),
    domElem "div" [
      tElemB (IncBox (\x -> show x `append` " words.") (appStateNumWords state))
    ],
    domElem "div" [
      tElemB (IncBox (\x -> show x `append` " definitions.") (appStateNumDefinitions state))
    ]
  ],
  domElem "div" (map (\word ->
                        domElem "div" [
                          tElem word,
                          domElem "div" [
                            domElem "ul" (map (\definition ->
                                                   domElem "li" [
                                                     tElem definition
                                                   ]
                                               )
                                               (mapDlookup word
                                                           (appStateDefinitions state)
                                               ))
                           ]
                        ]
                     )
                     (mapDkeys (appStateDefinitions state)))
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
