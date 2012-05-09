{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
module Bweb where
import B
import Inctime
import InctimeUtils

parse_request query = let word = lastInQueryString query "word"
                          definition = lastInQueryString query "definition"
                          action = lastInQueryString query "action"
                          evt = case action of
                                  "Add Definition" ->
                                     NewDefinitionInput word definition
                                  "Add Word" ->
                                     NewWordInput word
                      in BuiltinList_incrementalised_build_using_1 evt
  

