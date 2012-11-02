module InctimeUtils where

import Inctime
import Data.Maybe (fromJust)
import Data.List (intersperse)
import InctimeHtml

lastInQueryString :: [(String, Maybe String)] -> String -> String
lastInQueryString query name = case filter (\(n, v) -> n == name) query of
                                 [] -> error ("nothing for " ++ name ++ " in " ++ (show query))
                                 lookup_matches -> (fromJust . snd . last) lookup_matches

renderHtml :: Dom -> String
renderHtml (Element n attrs children) = concat ["<", n, " ", renderAttrs attrs, 
                                                ">",
                                                concatMap renderHtml children,
                                                "</", n, ">"]
renderHtml (TextElement s) = "<span>" ++ s ++ "</span>"
renderHtml (TextElementBox (IncBox f v)) = concat ["<span data-incbox=\"",
                                                   "UNDEFINED-JUST-NOW-SORRY",
                                                   "\">",
                                                   f v,
                                                   "</span>"]
renderAttrs = concat . (intersperse " ") . (map $ \(Attr a v) -> concat [a, "=\"", v, "\""])
