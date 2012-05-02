
module Main where

import B
import InctimeWeb
import Bweb

main = runApp parse_request (app_state ["hello"]) app_state_incrementalised page_view 
