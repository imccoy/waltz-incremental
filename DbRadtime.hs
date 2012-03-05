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

import Radtime (Char_incrementalised, Int_incrementalised, ZMZN_incrementalised (..), processRequest)

data DbAddressComponent = DbAddressComponent String Int

type DbAddress = [DbAddressComponent]

class DbIncrementalised incrementalised where
  applyDbInputChange :: incrementalised -> DbAddress -> IO ()


data DbStrategy = Inline | Separate
  deriving (Eq)

type DbStructureConstructor =  (String, [String])
type DbStructure = [(String, DbStrategy, [DbStructureConstructor])]
type DbStructureRow = (String, DbStrategy, [DbStructureConstructor])


appendAddress :: DbAddress -> String -> Int -> DbAddress
appendAddress address constructor fieldIndex = address ++ newPiece
  where newPiece = [DbAddressComponent constructor fieldIndex]

rootAddress = []

instance DbIncrementalised Char_incrementalised where
  applyDbInputChange change address = error "not implemented"

instance DbIncrementalised Int_incrementalised where
  applyDbInputChange change address = error "not implemented"

instance (DbIncrementalised elem_incrementalised) => 
            DbIncrementalised (ZMZN_incrementalised elem elem_incrementalised) where
  applyDbInputChange (ZMZN_incrementalised) address = error "not implemented"
  applyDbInputChange (ZC_incrementalised elem_incrementalised list_incrementalised) address = do
    applyDbInputChange elem_incrementalised (appendAddress address "ZC_incrementalised" 0)
  applyDbInputChange change address = error "not implemented"

prepareDb :: DbStructure -> IO SQLiteHandle
prepareDb structure = do
  handle <- openConnection "db"
  mapM_ (prepareTable handle structure) structure
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
          where (member_name, member_strategy, member_cons) = lookup_in_structure member
        separate_column member_name n = prepend_to_names (member_name ++ "__" ++ (show n)) $ [("type", "text"), ("id", "integer")]
        inline_column member_name member_cons n = prepend_to_names (member_name ++ "__" ++ (show n)) $ concat $ map inline_column_con member_cons
        inline_column_con (con_name, members) = prepend_to_names con_name $ con_columns (con_name, members)
        lookup_in_structure "" = ("anything", Separate, [])
        lookup_in_structure s = case find (\(a, _, _) -> a == s) structure of
                                   Just a -> a
                                   otherwise -> error ("Couldn't find structure for " ++ s)
        prepend_to_names :: String -> [(String, String)] -> [(String, String)]
        prepend_to_names s = map (\(a, b) -> (s ++ "__" ++ a, b))

         

app parse_request state incrementalised_state_function representationFunction request = do
  request_body_chunks <- EL.consume
  let request_body_query = parseQuery $ B.concat request_body_chunks
  let maybe_input_change = processRequest parse_request request request_body_query
  case maybe_input_change of
    Nothing             -> return ()
    (Just input_change) -> let output_change = incrementalised_state_function input_change
                            in tryIO $ applyDbInputChange output_change rootAddress
  let response = responseLBS
              status200
              [("Content-Type", B8.pack "text/html")]
              (renderHtml $ representationFunction state)
  return response

runAppDb parse_request initial_state db_structure incrementalized_state_function page_view = do
    putStrLn $ "http://localhost:8080/"
    prepareDb db_structure
    run 8080 $ app parse_request initial_state incrementalized_state_function page_view 


