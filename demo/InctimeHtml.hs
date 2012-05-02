module InctimeHtml where

import Inctime

data Attr = Attr String String

data Dom = Element String [Attr] [Dom]
         | TextElement String

domElem :: String -> [Dom] -> Dom
domElem s c = Element s [] c
elemA :: String -> [Attr] -> [Dom] -> Dom
elemA s as c = Element s as c
tElem :: String -> Dom
tElem s = TextElement s
