{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, MultiParamTypeClasses, UndecidableInstances #-}

module DbRadtime where

import Control.Monad
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

import Radtime (Char_incrementalised, Int_incrementalised, ZMZN_incrementalised (..), processRequest, applyInputChange)

data DbAddressComponent = DbAddressComponent String Int

type DbAddress = [DbAddressComponent]

class DbIncrementalised incrementalised where
  applyDbInputChange :: SQLiteHandle -> DbStructure -> incrementalised -> DbAddress -> IO ()

class DbInitialise base where
  setInitialValue :: SQLiteHandle -> DbStructure -> base -> IO [(String, String)]


data DbStrategy = Inline | Separate
  deriving (Eq)

type DbStructureConstructor =  (String, [String])
type DbStructure = [(String, DbStrategy, [DbStructureConstructor])]
type DbStructureRow = (String, DbStrategy, [DbStructureConstructor])


appendAddress :: DbAddress -> String -> Int -> DbAddress
appendAddress address constructor fieldIndex = address ++ newPiece
  where newPiece = [DbAddressComponent constructor fieldIndex]

rootAddress = []

atomicApplyDbInputChange reader shower handle structure change relative_address = let address@(table, column, condition) = find_absolute_address structure relative_address
                                                                                   in do putStrLn $ build_select column table condition
                                                                                         r <- execStatement handle $ build_select column table condition
                                                                                         let v = case r of
                                                                                                   Left e -> error $ "Couldn't select " ++ show address ++ ": " ++ e
                                                                                                   Right ((((column_name,value):_):_):_) -> value
                                                                                                   Right empty -> error $ "Couldn't select " ++ show address ++ ": was empty"
                                                                                         let new_v = applyInputChange change (reader v)
                                                                                         let update_sql = build_update table column (shower new_v) condition
                                                                                         putStrLn $ update_sql
                                                                                         execStatement_ handle update_sql >>= failIfError
              

instance DbIncrementalised Char_incrementalised where
  applyDbInputChange = atomicApplyDbInputChange (\a -> (head a) :: Char) (\a -> show a)

instance DbInitialise Char where
  setInitialValue handle structure c = return [("Char", show c)]
  
instance DbIncrementalised Int_incrementalised where
  applyDbInputChange = atomicApplyDbInputChange (\(Int a) -> (fromIntegral a) :: Int) (\a -> show a)

instance DbInitialise Int where
  setInitialValue handle structure n = return [("Int", show n)]
  
instance (DbIncrementalised elem_incrementalised) => 
            DbIncrementalised (ZMZN_incrementalised elem elem_incrementalised) where
  applyDbInputChange handle structure (ZMZN_incrementalised) address = error "not implemented"
  applyDbInputChange handle structure (ZC_incrementalised elem_incrementalised list_incrementalised) address = do
    applyDbInputChange handle structure elem_incrementalised (appendAddress address "ZC_incrementalised" 0)
  applyDbInputChange handle structure change address = error "not implemented"

instance DbInitialise [elem] where
  setInitialValue handle structure n = return []

prepareDb structure initial_value = do
  handle <- openConnection "db"
  mapM_ (prepareTable handle structure) structure
  initial_value_columns <- setInitialValue handle structure initial_value
  void $ perform_insert handle "AppState" initial_value_columns
  return handle

failIfError Nothing = return ()
failIfError (Just m) = error m

prepareTable :: SQLiteHandle -> DbStructure -> DbStructureRow -> IO ()
prepareTable handle structure (name, strategy, cons) = execStatement_ handle ("CREATE TABLE " ++ name ++ " (id INTEGER PRIMARY KEY AUTOINCREMENT, " ++ columns_sql ++ ")") >>= failIfError
  where columns_sql = concat $ intersperse "," columns_sql'
        columns_sql' = map (\(a, b) -> a ++ " " ++ b) columns
        columns = concat $ map con_columns cons
        con_columns (con_name, members) = concat $ zipWith (con_member_columns con_name) members [0..]

        con_member_columns _ "Int" n = [("Int__" ++ show n, "integer")]
        con_member_columns _ "ZMZN" n = separate_column "ZMZN" n
        con_member_columns con_name member n
           | member_strategy == Inline = inline_column member_name member_cons n
           | member_strategy == Separate = separate_column member_name n
          where (member_name, member_strategy, member_cons) = lookup_in_structure structure member
        separate_column member_name n = prepend_to_names (member_name ++ "__" ++ (show n)) $ [("type", "text"), ("id", "integer")]
        inline_column member_name member_cons n = prepend_to_names (member_name ++ "__" ++ (show n)) $ concat $ map inline_column_con member_cons
        inline_column_con (con_name, members) = prepend_to_names con_name $ con_columns (con_name, members)

lookup_in_structure structure "" = ("anything", Separate, [])
lookup_in_structure structure s = case find (\(a, _, _) -> a == s) structure of
                           Just a -> a
                           otherwise -> error ("Couldn't find structure for " ++ s)

prepend_to_names :: String -> [(String, String)] -> [(String, String)]
prepend_to_names s = map (\(a, b) -> (s ++ "__" ++ a, b))


setInitialValue' :: SQLiteHandle -> DbStructure -> String -> [IO [(String, String)]] -> IO [(String, String)]
setInitialValue' handle structure type_name actions = 
  let (member_name, member_strategy, member_cons) = lookup_in_structure structure type_name
   in case member_strategy of
        Inline -> do columns <- sequence actions
                     return $ concat $ zipWith (\ns n -> map (\(name, value) -> (name ++ "__" ++ (show n), value)) ns) columns [0..]
        otherwise -> return []

perform_insert handle table values = let cols_sql = concat $ intersperse ", " $ map fst values
                                         vals_sql = concat $ intersperse ", " $ map (\a -> "'" ++ snd a ++ "'") values
                                         sql = "INSERT INTO " ++ table ++ " (" ++ cols_sql ++ ") VALUES (" ++ vals_sql ++ ")"
                                      in do putStrLn sql
                                            execStatement_ handle sql

build_select a b "" = "SELECT " ++ a ++ " FROM " ++ b
build_select a b c = (build_select a b "") ++ " WHERE " ++ c

build_update table column value "" = "UPDATE " ++ table ++ " SET " ++ column ++ " = " ++ value
build_update table column value condition = build_update table value column "" ++ " WHERE " ++ condition


find_absolute_address _ [] = error "find_absolute_address: no root"
find_absolute_address structure ((DbAddressComponent rootCon fieldIndex):address) = foldl (find_absolute_address' structure) initial address
  where initial = (rootCon, (constructor !! fieldIndex) ++ "__" ++ (show fieldIndex), "")
        (_, _, constructors) = lookup_in_structure structure rootCon
        constructor = snd $ head $ constructors -- should be a lookup based on part of fieldIndex, not currently captured

find_absolute_address' structure (table, field, condition) (DbAddressComponent addr_con field_index)
    | strategy == Inline = (table, field ++ "__" ++ (show field_index), condition)
    | strategy == Separate = (name, "", "id = (" ++ (build_select (field ++ "__" ++ (show field_index)) table condition) ++ ")") 
  where (name, strategy, constructors) = lookup_in_structure structure addr_con

app handle structure parse_request state incrementalised_state_function representationFunction request = do
  request_body_chunks <- EL.consume
  let request_body_query = parseQuery $ B.concat request_body_chunks
  let maybe_input_change = processRequest parse_request request request_body_query
  case maybe_input_change of
    Nothing             -> return ()
    (Just input_change) -> let output_change = incrementalised_state_function input_change
                            in tryIO $ applyDbInputChange handle structure output_change rootAddress
  let response = responseLBS
              status200
              [("Content-Type", B8.pack "text/html")]
              (renderHtml $ representationFunction state)
  return response

runAppDb parse_request initial_state db_structure incrementalized_state_function page_view = do
    putStrLn $ "http://localhost:8080/"
    handle <- prepareDb db_structure initial_state
    run 8080 $ app handle db_structure parse_request initial_state incrementalized_state_function page_view 


