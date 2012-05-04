module InctimeHtml where

import Inctime

data Attr = Attr String String

data Dom = Element String [Attr] [Dom]
         | TextElement String
         | TextElementBox (IncBox String)

domElem :: String -> [Dom] -> Dom
domElem s c = Element s [] c
elemA :: String -> [Attr] -> [Dom] -> Dom
elemA s as c = Element s as c
tElem :: String -> Dom
tElem s = TextElement s
tElemB :: IncBox String -> Dom
tElemB b = TextElementBox b
