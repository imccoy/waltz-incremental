{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
module Bprime.Web where
import BPrime
import BPrimeInstances
import Radtime
import InctimeUtils

parse_request query = let word = lastInQueryString query "word"
                       in ZC_incrementalised_build_using_1 word
  
