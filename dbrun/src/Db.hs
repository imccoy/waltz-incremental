{-# LANGUAGE TupleSections, ScopedTypeVariables #-}
module Db where

import Types

import Control.Exception (handle, ErrorCall)
import Control.Monad (when, forM, liftM)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isAlphaNum)
import Data.List (intersperse)
import System.Directory (doesFileExist, removeFile)
import System.Exit (exitFailure)

import Data.List.Split (splitOn)
import Safe

import Database.HDBC
import Database.HDBC.Sqlite3

import Text.ParserCombinators.Parsec.Prim (parse)
import Language.Core.Core
import Language.Core.CoreUtils
import Language.Core.ParsecParser

dbfile = "dbfile.db"

db_create_stmt = "CREATE TABLE everything (id INTEGER PRIMARY KEY, " ++ 
                                         "type INTEGER NOT NULL, " ++
                                         "string TEXT, " ++
                                         "integer INTEGER, " ++
                                         "dcon TEXT, " ++
                                         "args TEXT);"
db_value_type_thunk = 0
db_value_type_dcon = 1
db_value_type_integral = 2
db_value_type_char = 3
db_value_type_string = 4

connection = liftIO $ do
  connectSqlite3 dbfile

initDb env heap value = do
  doesFileExist dbfile >>= (`when` (removeFile dbfile))
  conn <- connection
  run conn db_create_stmt []
  commit conn
  r <- addDb conn env heap value
  commit conn
  disconnect conn
  return r

returnId :: (IConnection conn) => conn -> IO Integer
returnId conn = do
  resultSet <- quickQuery' conn "SELECT last_insert_rowid()" []
  let resultRow = head resultSet
  let resultId = head resultRow
  return (fromSql resultId :: Integer)

addDb :: (IConnection conn) => conn -> Env -> Heap -> Value -> IO Integer
addDb conn initialEnv heap (DataValue tag args) = do
  db_args <- addDbArgs conn initialEnv heap args
  run conn
      "INSERT INTO everything (type, dcon, args) VALUES (?, ?, ?)"
      [toSql db_value_type_dcon
      ,toSql $ show tag
      ,toSql $ db_args]
  returnId conn
addDb conn initialEnv heap (IntegralValue integer) = do
  run conn "INSERT INTO everything (type, integer) VALUES (?, ?)"
           [toSql db_value_type_integral
           ,toSql integer]
  returnId conn
addDb conn initialEnv heap (CharValue char) = do
  run conn "INSERT INTO everything (type, string) VALUES (?, ?)"
           [toSql db_value_type_char
           ,toSql [char]]
  returnId conn
addDb conn initialEnv heap (StringValue string) = do
  run conn "INSERT INTO everything (type, string) VALUES (?, ?)"
           [toSql db_value_type_string
           ,toSql string]
  returnId conn
addDb conn initialEnv heap (DatabaseValue id) = return id
addDb conn initialEnv heap (Thunk env exp args) = do
  let (expString, args') = marshalExp conn initialEnv heap env exp args
  db_args <- addDbArgs conn initialEnv heap args'
  run conn "INSERT INTO everything (type, string, args) VALUES (?, ?, ?)"
           [toSql db_value_type_thunk
           ,toSql expString
           ,toSql db_args]
  returnId conn
  

addDbArgs conn initialEnv heap args = do
  let arg_values = map (heapGet' heap) args
  inserted_args <- mapM (addDb conn initialEnv heap) arg_values
  return $ concat $ intersperse "," (map show inserted_args)

marshalExp :: (IConnection conn) => conn ->
                                    Env ->
                                    Heap ->
                                    Env->
                                    InterpExp ->
                                    [HeapValue] ->
                                    (String, [HeapValue])
marshalExp conn _ _ _ (RtExp _ mod name _) args = ('b':(show (Just mod, name)), args)
marshalExp conn initialEnv heap env (CoreExp exp) args = ('c':(show exp'), args')
  where fvs = [e | e <- freeVars exp, e `notElem` (envKeys initialEnv)]
        exp' = foldr (Lam . Vb . (,Tvar "lostTy") . snd) exp fvs
        args' = (map lookupArg fvs) ++ args
        lookupArg n = case envLookup n env of
                        Just a -> a
                        otherwise -> error $ "marshalExp can't resolve var " ++ show n
  

applyChangeDb _ _ _ = return undefined

retrieveValue id = do
  conn <- connection
  result <- quickQuery' conn "SELECT * FROM everything WHERE id = ?" [toSql id]
  return $ retrieveValue' $ headNote ("No value in db at " ++ show id) result
retrieveValue' (id:ty:string:integer:dcon:dcon_args:[])
   = marshalValue (fromSql ty :: Integer)
                  (fromSql string :: String)
                  (fromSql integer :: Integer)
                  (fromSql dcon :: String)
                  (fromSql dcon_args :: String)


marshalValue ty string integer dcon args
 | ty == db_value_type_dcon     = do arg_refs <- mapM (heapAdd . DatabaseValue)
                                                      =<< arg_ints "dcon arg"
                                     dcon' <- parseDcon dcon
                                     return $ DataValue dcon' arg_refs
 | ty == db_value_type_thunk    = do arg_refs <- mapM (heapAdd . DatabaseValue)
                                                      =<< arg_ints "thunk arg"
                                     exp <- liftM CoreExp (load_exp string)
                                     return $ Thunk envEmpty exp arg_refs
 | ty == db_value_type_integral = return $ IntegralValue integer
 | ty == db_value_type_char     = return $ CharValue $ head string
 | ty == db_value_type_string   = return $ StringValue string
 where 
   arg_strings = splitOn "," args
   arg_ints msg = forM arg_strings (parseWithMessage msg) :: HeapM [Integer]
   parseWithMessage msg s = liftIO $ handle (stop msg s)
                                            (let s' = read s
                                              in s' `seq` return s')
   stop msg s (e :: ErrorCall) = do putStrLn $ "couldn't parse " ++ s
                                                ++ " as " ++ msg
                                    exitFailure
   load_exp ('b':string) = liftM Var $ parseWithMessage string "builtin thunk"
   load_exp ('c':string) = case parse coreFullExp ("db exp") string of
                             Left err -> error $ show err
                             Right exp -> return exp
parseDcon ('(':s) = let [mnameString, '"':dconString'] = splitOn "," s
                        dconString = takeWhile (/= '"') dconString'
                        mname = parseMname mnameString
                     in mname `seq` return (mname, dconString)
parseMname "Nothing" = Nothing
parseMname ('J':'u':'s':'t':' ':s) = Just $ parseAnMname s
parseAnMname s = let [package, s1] = splitOn ":" s
                     modulesAndNameString = takeWhile isAlphaNum s1
                     modulesAndName = splitOn "zi" modulesAndNameString
                     (name:rmodules) = reverse modulesAndName
                     modules = reverse rmodules
                  in M (P package, modules, name)

