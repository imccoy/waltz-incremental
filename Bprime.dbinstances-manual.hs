module Binstances.Manual where

import B
import DbRadtime

import Database.SQLite

instance LoadableState AppState where
  load_state handle structure id = do
    let sql = build_select_multi ["Int__0__id", "ZMZN__1__id", "ZMZN__2__id"] "AppState" ("id = " ++ show id)
    putStrLn sql
    r <- execStatement handle $ sql
    let (total_length_id, words_id, latest_word_id) = case r of
                                                     Left e -> error $ "Couldn't select AppState " ++ show id ++ ": " ++ e
                                                     Right [] -> error $ "Couldn't select AppState" ++ show id ++ ": was empty"
                                                     Right ((row:_):_) -> (snd $ row !! 0, snd $ row !! 1, snd $ row !! 2)
                                                     Right funky -> error $ "Couldn't select AppState" ++ show id ++ ": was funky " ++ (show funky)
    total_length <- load_state handle structure (read total_length_id)
    words <- load_state handle structure (read words_id)
    latest_word <- load_state handle structure (read latest_word_id)
    return $ AppState total_length words latest_word
 
