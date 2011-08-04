{-# LANGUAGE OverloadedStrings #-}
import Data.Enumerator (tryIO)
import qualified Data.Enumerator.List as EL
import Data.IORef
import Data.Maybe (fromJust)
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B

-- TODOS
-- How do you make two different haps using two different columns of a html table?
-- How do you make two different haps that submit at the same time? Two different haps that submit independently?


-- framework stuff
import Debug.Trace


-- framework: page display
follow_to source target = FuncAssociationWalk source target
showValueAsString = FuncF (\(ValueEntity entity) -> Text $ show entity)
showEntryText = FuncF getText
  where getText (ValueEntity (EntityEntry (Entry text))) = Text text
        getText e = error $ "can't get text " ++ (show e)


data Value = Tag String [(String, String)] Value
           | Text String
           | ValueEntity Entity
           | ValueFuncCall Func [Value]
           | Values [Value]
           | ActionReceiverValue ActionReceiver [Value]
           | ActionReceiverInput ActionReceiverField ActionReceiverControl
  deriving (Show)

data Func = FuncAssociationWalk String String | FuncMap (Value -> Value) | FuncF (Value -> Value)

instance Show Func where
  show (FuncAssociationWalk source target) = "func: " ++ source ++ " -> " ++ target
  show _ = "func"


drawPage :: Value -> EntityData -> String
drawPage val dta = drawPageValue val 0
  where
    line indent string = concat $ (replicate indent "  ") ++ [string, "\n"]
    drawPageValue (Tag name attrs child) lvl = line lvl ("<" ++ name ++ (attrString attrs) ++  ">") ++ 
                                               drawPageValue child (lvl + 1) ++
                                               line lvl ("</" ++ name ++ ">")
      where attrString attrs = concat $ map (\(name, value) -> concat [" ", name, "=\"", value, "\""]) attrs
    drawPageValue (Text text) lvl = line lvl text
    drawPageValue (Values values) lvl = concat $ map (\v -> drawPageValue v lvl) values 
    drawPageValue func_call@(ValueFuncCall _ _) lvl = drawPageValue (eval func_call) lvl
    drawPageValue (ValueEntity entity) lvl = error ("can't draw value: entity " ++ (show entity))
    drawPageValue (ActionReceiverValue actionReceiver values) lvl = drawPageValue (Tag "form" [("method", "post")] $ Values (input:values)) lvl
       where input = Tag "input" [("type", "hidden"), ("name", "actionReceiver"), ("value", actionReceiverName actionReceiver)] $ Values []
    drawPageValue (ActionReceiverInput actionReceiverField Textfield) lvl = drawPageValue (Tag "input" [("name", fieldName actionReceiverField)] $ Values []) lvl
    drawPageValue (ActionReceiverInput actionReceiverField (Dropdown options)) lvl = drawSelect actionReceiverField options "" lvl
    drawPageValue (ActionReceiverInput actionReceiverField (DropdownV options def)) lvl = drawSelect actionReceiverField options def lvl
    drawSelect actionReceiverField options def lvl = drawPageValue (Tag "select" [("name", fieldName actionReceiverField)] $ Values optionTags) lvl
       where optionTags = map (\option -> Tag "option" (attrs option) $ Text option) options
             attrs option = if option == def then [("selected", "selected")] else []

    eval :: Value -> Value
    eval (ValueFuncCall func args) = invoke func args
    eval v = v

    invoke :: Func -> [Value] -> Value
    invoke func args = invoke' func (map eval args)
      where
        invoke' (FuncAssociationWalk source_type attr) [ValueEntity entity] = Values $ map ValueEntity $ lookup_related dta entity
        invoke' (FuncAssociationWalk source_type attr) vs = error $ "Can't walk association over " ++ (show vs)
        invoke' (FuncMap f) [Values vs] = Values $ map f vs
        invoke' (FuncMap f) vs = Values $ map f vs
        invoke' (FuncF f) [v] = f v
        invoke' (FuncF f) [] = error "No arguments to singular function"
        invoke' (FuncF f) _ = error "Too many arguments to singular function"




-- framework: data store

type EntityData = [EntityStore]
emptyData = []
data EntityStore = EntityStoreEntry Entity Entity

build_data :: [EntityData -> EntityData] -> EntityData
build_data fs = build_data' emptyData fs
  where build_data' d [] = d
        build_data' d (f:fs) = build_data' (f d) fs

add_entity parent child d = (EntityStoreEntry parent child):d

lookup_related :: EntityData -> Entity -> [Entity]
lookup_related dta entity = map (\(EntityStoreEntry a b) -> b) $ (filter matchesEntity) dta
  where matchesEntity :: EntityStore -> Bool
        matchesEntity (EntityStoreEntry a b) = a == entity

-- framework: user input

data Ensurer = EnsureIsOneOf [String] | EnsureIsNotEmpty
  deriving (Show)

data ActionReceiverField = ActionReceiverField { fieldName :: String, validators :: [Ensurer] }
  deriving (Show)

data ActionReceiver = ActionReceiver String [ActionReceiverField] ((ActionReceiverField -> String) -> Action)
actionReceiverName (ActionReceiver name _ _) = name
actionReceiverBuilder (ActionReceiver _ _ builder) = builder

data ActionReceiverControl = Textfield | Dropdown [String] | DropdownV [String] String
  deriving (Show)

instance Show ActionReceiver where
  show (ActionReceiver name fields func) = "ActionReceiver " ++ name ++ " " ++ (show fields)

-- http

app :: IORef EntityData -> Application 
app db request = do current_db <- tryIO $ readIORef db
                    request_body_chunks <- EL.consume
                    let request_body_query = parseQuery $ B.concat request_body_chunks
                    let action = processRequest request current_db request_body_query
                    let actions = actionsFor action
                    let (response, new_db) = processActions actions current_db
                    tryIO $ writeIORef db new_db
                    return response

processRequest request current_db request_body_query = case parseMethod $ requestMethod request of
                                                         (Right POST) ->  actionReceiverToAction request_body_query
                                                         otherwise -> defaultAction

lastInQueryString :: Query -> String -> String
lastInQueryString query name = case filter (\(n, v) -> n == (B8.pack name)) $ query of
                                 []             -> error "nothing for " ++ name ++ " in " ++ (show query)
                                 lookup_matches -> B8.unpack $ fromJust $ snd $ last $ lookup_matches

actionReceiverToAction request_body_query = let action_receiver_name = lastInQueryString request_body_query "actionReceiver"
                                                action_receiver = last $ filter (\h -> (actionReceiverName h) == action_receiver_name) allActionReceivers
                                                getter action_receiver_field = lastInQueryString request_body_query $ fieldName action_receiver_field
                                             in (actionReceiverBuilder action_receiver) getter

processActions actions current_db = processActions' actions current_db Nothing
  where
    processActions' ((AddPropertyOn entity property):actions) current_db page = processActions' actions (add_entity entity property current_db) page
    processActions' ((ShowPage page):actions) current_db Nothing = processActions' actions current_db (Just page)
    processActions' [] current_db (Just page) = (response, current_db)
      where response = responseLBS
                                status200
                                [("Content-Type", B8.pack "text/html")]
                                (LB8.pack $ drawPage (wrap_in_html $ page) current_db)

main = do
    putStrLn $ "http://localhost:8080/"
    db <- newIORef sample_data
    run 8080 $ app db

-- application stuff

data Section = Good | Bad | Confusing
  deriving (Show, Read, Eq)
data Entry = Entry String
  deriving (Show, Eq)

data Entity = EntitySection Section | EntityEntry Entry
  deriving (Show, Eq)

data Action = AddPropertyOn Entity Entity | ShowPage Value | NewEntry Section String | ShowRetro Section


defaultAction = ShowRetro Good

actionsFor (ShowRetro section) = [ShowPage $ retro_page section]
actionsFor (NewEntry section text) = [AddPropertyOn (EntitySection section) (EntityEntry $ Entry text), ShowPage $ retro_page section]

possibleSectionStrings = ["Good", "Bad", "Confusing"]

newEntryActionReceiverSection = ActionReceiverField { fieldName = "section", validators = [EnsureIsOneOf possibleSectionStrings] }
newEntryActionReceiverText = ActionReceiverField { fieldName = "text", validators = [EnsureIsNotEmpty] }
newEntryActionReceiver = ActionReceiver "newEntry" [newEntryActionReceiverSection, newEntryActionReceiverText] (\getter -> NewEntry (read $ getter (newEntryActionReceiverSection)) (getter newEntryActionReceiverText))

allActionReceivers = [newEntryActionReceiver]


retro_entry entry = Tag "span" [] $ ValueFuncCall showEntryText [entry]

retro_entries section = Tag "div" [] $ Values [
                                      (Tag "h2" [] $ ValueFuncCall showValueAsString [section]),
				      (ValueFuncCall (FuncMap retro_entry) [ValueFuncCall ("Section" `follow_to` "Entry") [section]])]

retro = Tag "div" [] $ Values [retro_entries(ValueEntity $ EntitySection Good),
                               retro_entries(ValueEntity $ EntitySection Bad),
                               retro_entries(ValueEntity $ EntitySection Confusing)]
           
sample_data = build_data
                     [
                      add_entity (EntitySection Good) (EntityEntry $ Entry "It's Okay"),
                      add_entity (EntitySection Bad) (EntityEntry $ Entry "It's Ugly")
                      ]

retro_page initial_section = Values [(Tag "h1" [] $ Text "Retro"),
                                     retro,
                                     ActionReceiverValue newEntryActionReceiver [
                                       ActionReceiverInput newEntryActionReceiverSection (DropdownV possibleSectionStrings $ show initial_section),
                                       ActionReceiverInput newEntryActionReceiverText Textfield
                                     ]]

wrap_in_html body = Tag "html" [] $ Values [
                                    (Tag "head" [] $ Tag "title" [] $ Text "Waltz App"),
                                    (Tag "body" [] body)]


