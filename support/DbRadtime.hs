{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, MultiParamTypeClasses, UndecidableInstances #-}

module DbRadtime where

import Control.Monad
import Data.Data
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import qualified Data.Enumerator.List as EL
import Data.Enumerator (tryIO)
import Data.List
import Data.Maybe (fromJust, maybeToList)
import Data.IORef
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Text.Blaze.Renderer.Utf8 (renderHtml)

import Database.SQLite
import Debug.Trace

import Radtime (Char_incrementalised, Int_incrementalised, ZMZN_incrementalised (..), processRequest, applyInputChange)

data DbAddressComponent = DbAddressComponent String Int
  deriving (Show)

type DbAddress = [DbAddressComponent]

class DbIncrementalised incrementalised where
  applyDbInputChange :: SQLiteHandle -> DbStructure -> incrementalised -> DbAddress -> IO ()

class DbInitialise base where
  setInitialValue :: SQLiteHandle -> DbStructure -> base -> IO [(String, String)]


data DbStrategy = Inline | Separate
  deriving (Eq, Show)

type DbStructureConstructor =  (String, [String])
type DbStructure = [(String, DbStrategy, [DbStructureConstructor])]
type DbStructureRow = (String, DbStrategy, [DbStructureConstructor])

default_db_structure = [("ZMZN", Separate, [("ZC", ["", "ZMZN"]),
                                            ("ZMZN", [])]),
                        ("Char", Inline, [("", ["char"])]),
                        ("Int", Inline, [("", ["int"])])
                       ]

appendAddress :: DbAddress -> String -> Int -> DbAddress
appendAddress address constructor fieldIndex = address ++ newPiece
  where newPiece = [DbAddressComponent constructor fieldIndex]

rootAddress = []

atomicApplyDbInputChange name reader shower handle structure change relative_address = do
  let address@(table, column, condition) = find_absolute_address structure relative_address
  putStrLn $ build_select column table condition
  r <- execStatement handle $ build_select column table condition
  let old_v_id = case r of
                   Left e -> error $ "Couldn't select " ++ show address ++ ": " ++ e
                   Right ((((column_name,value):_):_):_) -> value
                   Right empty -> error $ "Couldn't select " ++ show address ++ ": was empty"
  r' <- execStatement handle $ build_select name name $ "id = " ++ old_v_id
  let old_v = case r' of
                   Left e -> error $ "Couldn't select " ++ show address ++ " value: " ++ e
                   Right ((((column_name,value):_):_):_) -> value
  let new_v = applyInputChange change (reader old_v)
  new_v_row <- setInitialValue handle structure new_v
  let update_sql = build_update table column (snd $ head new_v_row) condition
  putStrLn $ update_sql
  execStatement_ handle update_sql >>= failIfError
              

instance DbIncrementalised Char_incrementalised where
  applyDbInputChange = atomicApplyDbInputChange "Char" (\a -> (head a) :: Char) (\(a :: Char) -> show a)

instance DbInitialise Char where
  setInitialValue handle structure c = do perform_insert handle "Char" [("Constructor", "Char"), ("Char", [c])]
                                          id <- getLastRowID handle
                                          return [("id", show id), ("type", "Char")]
  
instance DbIncrementalised Int_incrementalised where
  applyDbInputChange = atomicApplyDbInputChange "Int" (\(Int a) -> (fromIntegral a) :: Int) (\(a :: Int) -> show a)

instance DbInitialise Int where
  setInitialValue handle structure n = do perform_insert handle "Int" [("Constructor", "Int"), ("Int", show n)]
                                          id <- getLastRowID handle
                                          return [("id", show id), ("type", "Int")]
  
instance (DbIncrementalised elem_incrementalised, DbInitialise elem) => 
            DbIncrementalised (ZMZN_incrementalised elem elem_incrementalised) where
  applyDbInputChange handle structure (ZMZN_incrementalised) address = error "not implemented A" 
  applyDbInputChange handle structure (ZC_incrementalised elem_incrementalised list_incrementalised) address = do
    applyDbInputChange handle structure elem_incrementalised (appendAddress address "ZC_incrementalised" 0)

  applyDbInputChange handle structure (ZC_incrementalised_build_using_1 elem) relative_address = do
    let address@(table, column, condition) = find_absolute_address structure relative_address
    r <- execStatement handle $ build_select column table condition
    let old_head_id = case r of
              Left e -> error $ "Couldn't select " ++ show address ++ ": " ++ e
              Right ((((column_name,value):_):_):_) -> value
              Right empty -> error $ "Couldn't select " ++ show address ++ ": was empty"
    elem_columns <- setInitialValue handle structure elem
    perform_insert handle "ZMZN" $ [("Constructor", "ZC"), ("ZMZN__1__id", old_head_id), ("ZMZN__1__type", "ZMZN")] ++ (prepend_to_names "anything__0" elem_columns)
    new_head_id <- getLastRowID handle
    let sql = build_update table column (show new_head_id) condition
    putStrLn sql
    execStatement_ handle sql >>= failIfError

  applyDbInputChange handle structure (ZMZN_incrementalised_identity) address = return ()
  applyDbInputChange handle structure (ZMZN_incrementalised_hoist) address = error "not implemented C"
  applyDbInputChange handle structure (ZMZN_incrementalised_replace xs) relative_address = do
    let address@(table, column, condition) = find_absolute_address structure relative_address
    elem_columns <- setInitialValue handle structure xs
    let new_head_id = fromJust $ lookup "id" elem_columns
    let sql = build_update table column new_head_id condition
    putStrLn sql
    execStatement_ handle sql >>= failIfError
  applyDbInputChange handle structure change address = error "not implemented B"

instance (DbInitialise elem) => DbInitialise [elem] where
  setInitialValue handle structure [] = setInitialValue' handle structure "ZMZN" "ZMZN" []
  setInitialValue handle structure (h:t) = setInitialValue' handle structure "ZMZN" "ZC" [setInitialValue handle structure h, setInitialValue handle structure t]

prepareDb structure initial_value = do
  handle <- openConnection "db"
  mapM_ (prepareTable handle structure) structure
  setInitialValue handle structure initial_value
  return handle

failIfError Nothing = return ()
failIfError (Just m) = error m

prepareTable :: SQLiteHandle -> DbStructure -> DbStructureRow -> IO ()
prepareTable handle structure (name, strategy, cons) = putStrLn sql >> execStatement_ handle sql >>= failIfError
  where sql = ("CREATE TABLE " ++ name ++ " (id INTEGER PRIMARY KEY AUTOINCREMENT, constructor TEXT, " ++ columns_sql ++ ")")
        columns_sql = concat $ intersperse "," columns_sql'
        columns_sql' = map (\(a, b) -> a ++ " " ++ b) columns
        columns = if length (snd $ head cons) == 1 && elem (head $ head $ snd $ head cons) ['a'..'z'] then [(name, head $ snd $ head cons)] else concat $ map con_columns cons
        con_columns (con_name, members) = concat $ zipWith (con_member_columns con_name) members [0..]

        con_member_columns con_name member n
           | member == "" = separate_column "anything" n
           | elem (head member) ['a'..'z'] = [("", member)]
           | otherwise = separate_column member_name n
          where (member_name, member_strategy, member_cons) = lookup_in_structure structure member
        separate_column member_name n = prepend_to_names (member_name ++ "__" ++ (show n)) $ [("type", "text"), ("id", "integer")]

lookup_in_structure structure "" = ("anything", Separate, [])
lookup_in_structure structure s = case find (\(a, _, _) -> a == s) structure of
                           Just a -> a
                           otherwise -> error ("Couldn't find structure for " ++ s)

prepend_to_names :: String -> [(String, String)] -> [(String, String)]
prepend_to_names s = map (\(a, b) -> (prepend s a, b))
  where prepend "" "" = ""
        prepend a "" = a
        prepend "" b = b
        prepend a b = a ++ "__" ++ b


setInitialValue' :: SQLiteHandle -> DbStructure -> String -> String -> [IO [(String, String)]] -> IO [(String, String)]
setInitialValue' handle structure type_name con_name actions = 
  do let (member_name, member_strategy, member_cons :: [DbStructureConstructor]) = lookup_in_structure structure type_name
     -- let con = maybe (error "Couldn't find con " ++ con_name ++ " for " ++ type_name) id $ (lookup con_name member_cons)
     let con = fromJust (lookup con_name member_cons)
     columns <- sequence actions
     con_columns <- prepare_con_columns con columns
     let flattened_columns = (("constructor", con_name)):(concat $ zipWith flattened_column con_columns [0..])
     reference_insert member_name flattened_columns type_name
  where flattened_column :: (String, [(String, String)]) -> Int -> [(String, String)]
        flattened_column (con_elem, name_values) n = map (\(name, value) -> (con_elem ++ "__" ++ show n ++ "__" ++ name, value)) name_values

        reference_insert table columns type_name = do perform_insert handle table columns
                                                      id <- getLastRowID handle
                                                      return [("id", show id), ("type", type_name)]

        prepare_con_columns con columns = sequence $ zipWith prepare_con_column con columns
        prepare_con_column "" columns
          | (fst $ head columns) == "id" = return ("anything", columns)
          | otherwise                    = do columns' <- reference_insert (fst $ head columns) columns (fst $ head columns)
                                              return ("anything", columns')
        prepare_con_column con_elem columns = return (con_elem, columns)
                           

perform_insert _ _ [] = return ()
perform_insert handle table values = let sql = build_insert table values
                                      in do putStrLn sql
                                            execStatement_ handle sql >>= failIfError

build_select a b c = build_select_multi [a] b c

build_select_multi a b "" = "SELECT " ++ (concat $ intersperse "," a) ++ " FROM " ++ b
build_select_multi a b c = (build_select_multi a b "") ++ " WHERE " ++ c

build_update table column value "" = "UPDATE " ++ table ++ " SET " ++ column ++ " = " ++ value
build_update table column value condition = build_update table value column "" ++ " WHERE " ++ condition

build_insert table [] = ""
build_insert table values = let cols_sql = concat $ intersperse ", " $ map fst values
                                vals_sql = concat $ intersperse ", " $ map (\a -> "'" ++ snd a ++ "'") values
                             in "INSERT INTO " ++ table ++ " (" ++ cols_sql ++ ") VALUES (" ++ vals_sql ++ ")"

find_absolute_address _ [] = error "find_absolute_address: no root"
find_absolute_address structure complete_address@((DbAddressComponent rootCon fieldIndex):address) = foldl (find_absolute_address' structure) initial address
  where initial = (rootCon, (constructor !! fieldIndex) ++ "__" ++ (show fieldIndex) ++ "__id", "")
        (_, _, constructors) = lookup_in_structure structure rootCon
        constructor = snd $ head $ constructors -- should be a lookup based on part of fieldIndex, not currently captured

find_absolute_address' structure (table, field, condition) (DbAddressComponent addr_con field_index)
    = (name, "", "id = (" ++ (build_select (field ++ "__" ++ (show field_index)) table condition) ++ ")") 
  where (name, strategy, constructors) = lookup_in_structure structure addr_con



class LoadableState a where
  load_state :: SQLiteHandle -> DbStructure -> Int -> IO a


instance (LoadableState a => LoadableState [a]) where
  load_state handle structure id = do
    let sql = build_select_multi ["constructor", "anything__0__id", "anything__0__type", "ZMZN__1__id"] "ZMZN" ("id = " ++ show id)
    putStrLn sql
    r <- execStatement handle $ sql
    let (constructor, elem_id, elem_type, tail_id) = case r of
                                          Left e -> error $ "Couldn't select ZMZN " ++ show id ++ ": " ++ e
                                          Right [] -> error $ "Couldn't select ZMZN" ++ show id ++ ": was empty"
                                          Right ((row:_):_) -> (snd $ row !! 0, snd $ row !! 1, snd $ row !! 2, snd $ row !! 3)
                                          Right e -> error $ "Couldn't select ZMZN" ++ show id ++ ": was funky " ++ (show e)
    case constructor of
      "ZMZN" -> return []
      "ZC"   -> do tail <- load_state handle structure (read tail_id)
                   elem <- load_state handle structure (read elem_id)
                   return $ elem:tail
      con    -> error $ "Unknown constructor for ZMZN " ++ con
                                                                                         

instance LoadableState Char where 
  load_state handle structure id = do
    let sql = build_select "Char" "Char" ("id = " ++ show id)
    putStrLn sql
    r <- execStatement handle $ sql
    let v = case r of
              Left e -> error $ "Couldn't select Char " ++ show id ++ ": " ++ e
              Right [] -> error $ "Couldn't select Char" ++ show id ++ ": was empty"
              Right ((row:_):_) -> snd $ row !! 0
    return $ head v
 
instance LoadableState Int where 
  load_state handle structure id = do
    let sql = build_select "Int" "Int" ("id = " ++ show id)
    putStrLn sql
    r <- execStatement handle $ sql
    let v = case r of
              Left e -> error $ "Couldn't select Int " ++ show id ++ ": " ++ e
              Right [] -> error $ "Couldn't select Int " ++ show id ++ ": was empty"
              Right ((row:_):_) -> snd $ row !! 0
              Right e -> error $ "Couldn't select Int " ++ show id ++ ": was funky " ++ show e
    return $ read v
 



app handle structure parse_request incrementalised_state_function representationFunction request = do
  request_body_chunks <- EL.consume
  let request_body_query = parseQuery $ B.concat request_body_chunks
  let maybe_input_change = processRequest parse_request request request_body_query
  case maybe_input_change of
    Nothing             -> return ()
    (Just input_change) -> let output_change = incrementalised_state_function input_change
                            in tryIO $ applyDbInputChange handle structure output_change rootAddress
  state <- tryIO $ load_state handle structure 1
  let response = responseLBS
              status200
              [("Content-Type", B8.pack "text/html")]
              (renderHtml $ representationFunction state)
  return response

runAppDb parse_request initial_state db_structure incrementalized_state_function page_view = do
    putStrLn $ "http://localhost:8080/"
    let db_structure' = default_db_structure ++ db_structure
    handle <- prepareDb db_structure' initial_state
    run 8080 $ app handle db_structure' parse_request incrementalized_state_function page_view 


