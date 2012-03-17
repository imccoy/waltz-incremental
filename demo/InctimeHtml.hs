module InctimeHtml where

import Radtime

data Attr = Attr String String

data Dom = Element String [Attr] [Dom]
         | TextElement String

domElem :: String -> [Dom] -> Dom
domElem s = Element s []
elemA :: String -> [Attr] -> [Dom] -> Dom
elemA = Element
tElem :: String -> Dom
tElem = TextElement



