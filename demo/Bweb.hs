{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
module Bweb where
import B
import Inctime
import InctimeUtils

parse_request query = let word = lastInQueryString query "word"
                      in BuiltinList_incrementalised_build_using_1 word
  

