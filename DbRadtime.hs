{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, MultiParamTypeClasses, UndecidableInstances, RankNTypes #-}

module DbRadtime where

import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import Data.Data
import qualified Data.Enumerator.List as EL
import Data.Enumerator (tryIO)
import Data.List
import Data.Maybe (fromJust, maybeToList, isJust)
import Data.IORef
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import qualified Text.Blaze
import Text.Blaze.Renderer.Utf8 (renderHtml)

import Debug.Trace
import Database.SQLite

import Radtime (Char_incrementalised (..), Int_incrementalised (..), ZMZN_incrementalised (..), processRequest, applyInputChange)

data DbAddressComponent = DbAddressComponent String Int
  deriving (Show)

type DbAddress = [DbAddressComponent]


data DbStrategy = Inline | Separate
  deriving (Eq, Show)
type DbStructureConstructor =  (String, [String])
type DbStructureRow = (String, DbStrategy, [DbStructureConstructor])
type DbStructure = [DbStructureRow]

class DbIncrementalised incrementalised where
  applyDbInputChange :: SQLiteHandle -> DbStructure -> incrementalised -> DbAddress -> IO ()


appendAddress :: DbAddress -> String -> Int -> DbAddress
appendAddress address constructor fieldIndex = address ++ newPiece
  where newPiece = [DbAddressComponent constructor fieldIndex]

rootAddress = []

atomicApplyDbInputChange reader shower handle structure change relative_address = let address@(table, column, condition) = trace (show relative_address) $ find_absolute_address structure relative_address 
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

instance DbIncrementalised Int_incrementalised where
  applyDbInputChange = atomicApplyDbInputChange (\(Int a) -> (fromIntegral a) :: Int) (\a -> show a)

instance (DbIncrementalised elem_incrementalised) => 
            DbIncrementalised (ZMZN_incrementalised elem elem_incrementalised) where
  applyDbInputChange handle structure (ZMZN_incrementalised) address = error "not implemented"
  applyDbInputChange handle structure (ZC_incrementalised elem_incrementalised list_incrementalised) address = do
    applyDbInputChange handle structure elem_incrementalised (appendAddress address "ZC_incrementalised" 0)
  applyDbInputChange handle structure change address = error "not implemented"

prepare_db :: DbStructure -> (forall a. (Show a, Data a) => a) -> IO SQLiteHandle
prepare_db structure initial_state = do
  handle <- openConnection "db"
  mapM_ (prepare_table handle structure) structure
  load_initial_state handle structure initial_state
  return handle

failIfError Nothing = return ()
failIfError (Just m) = error m

prepare_table :: SQLiteHandle -> DbStructure -> DbStructureRow -> IO ()
prepare_table handle structure (name, strategy, cons) = do putStrLn sql_statement
                                                           execStatement_ handle sql_statement >>= failIfError
  where sql_statement = "CREATE TABLE " ++ name ++ " (id INTEGER PRIMARY KEY AUTOINCREMENT, " ++ columns_sql ++ ")"
        columns_sql = concat $ intersperse "," columns_sql'
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
        prepend_to_names :: String -> [(String, String)] -> [(String, String)]
        prepend_to_names s = map (\(a, b) -> (s ++ "__" ++ a, b))

lookup_in_structure _ ""        = ("anything", Separate, [])
lookup_in_structure structure s = case find (\(a, _, _) -> a == s) structure of
                                    Just a -> a
                                    otherwise -> error ("Couldn't find structure for " ++ s)

load_initial_state :: SQLiteHandle -> DbStructure -> (forall a. (Data a, Show a) => a) -> IO ()
load_initial_state handle structure state = do let (initializers, member_name) = (initializers_for handle structure state)
                                               let inits = (zipWith (\a b -> a b) initializers  [0..]) :: [IO [(String, String)]]
                                               updates <- (fmap concat $ sequence $ inits) :: IO [(String, String)]
                                               let columns = concat $ intersperse "," $ map fst updates
                                               let values = concat $ intersperse "," $ map (\a -> "'" ++ snd a ++ "'") updates
                                               let insert_sql = ("INSERT INTO " ++ member_name ++ "(" ++ columns ++ ") VALUES (" ++ values ++ ")")
                                               putStrLn insert_sql
                                               execStatement_ handle insert_sql  >>= failIfError

initializers_for :: t -> DbStructure -> (forall a. (Data a, Show a) => a) -> ([Int -> IO [([Char], String)]], [Char])  
initializers_for handle structure state = (gmapQ (\a -> initializer handle structure structure_constr a)  state, member_name)
  where type_name = tail $ snd (span (/= '.') $ dataTypeName $ dataTypeOf state)
        member_structure@(member_name, member_strategy, member_cons) = lookup_in_structure structure $ type_name
        state_constr = showConstr $ toConstr state
        structure_constr = maybe (error $ "load_initial_state: couldn't find constructor \"" ++ state_constr ++ "\" in (" ++ type_name ++  ")++ " ++ show member_structure) id $ lookup state_constr member_cons

initializer :: t -> DbStructure -> [[Char]] -> (forall a. (Data a, Show a) => a) -> Int -> IO [([Char], String)]
initializer handle structure structure_constr param n = case dataTypeRep (dataTypeOf param) of
                                                          IntRep    -> return [(structure_constr_elem ++ "__" ++ (show n), show (convert param :: Int))]
                                                          AlgRep _  -> let (initializers, member_name) = initializers_for handle structure param
                                                                        in return [((structure_constr_elem) ++ "__" ++ (show n) ++ "_type", member_name)]
                                                          otherwise -> return []
  where structure_constr_elem = structure_constr !! n
        convert :: (Typeable a, Typeable b, Show b) => a -> b
        convert param = case cast param of
                         Just x -> x
                         Nothing -> error "failed cast"
 

find_absolute_address _ [] = error "find_absolute_address: no root"
find_absolute_address structure ((DbAddressComponent rootCon fieldIndex):address) = foldl (find_absolute_address' structure) initial address
  where initial = (rootCon, (constructor !! fieldIndex) ++ "__" ++ (show fieldIndex), "")
        (_, _, constructors) = lookup_in_structure structure rootCon
        constructor = snd $ head $ constructors -- should be a lookup based on part of fieldIndex, not currently captured

find_absolute_address' structure (table, field, condition) (DbAddressComponent addr_con field_index)
    | strategy == Inline = (table, field ++ "__" ++ (show field_index), condition)
    | strategy == Separate = (name, "", "id = (" ++ (build_select (field ++ "__" ++ (show field_index)) table condition) ++ ")") 
  where (name, strategy, constructors) = lookup_in_structure structure addr_con

build_select a b "" = "SELECT " ++ a ++ " FROM " ++ b
build_select a b c = (build_select a b "") ++ " WHERE " ++ c

build_update table column value "" = "UPDATE " ++ table ++ " SET " ++ column ++ " = " ++ value
build_update table column value condition = build_update table value column "" ++ " WHERE " ++ condition

app parse_request state handle db_structure incrementalised_state_function representationFunction request = do
  request_body_chunks <- EL.consume
  let request_body_query = parseQuery $ B.concat request_body_chunks
  let maybe_input_change = processRequest parse_request request request_body_query
  case maybe_input_change of
    Nothing             -> return ()
    (Just input_change) -> let output_change = incrementalised_state_function input_change
                            in tryIO $ applyDbInputChange handle db_structure output_change rootAddress
  let response = responseLBS
              status200
              [("Content-Type", B8.pack "text/html")]
              (renderHtml $ representationFunction state)
  return response

runAppDb parse_request initial_state db_structure incrementalized_state_function page_view = do
    putStrLn $ "http://localhost:8080/"
    handle <- prepare_db db_structure initial_state
    run 8080 $ app parse_request initial_state handle db_structure incrementalized_state_function page_view 


