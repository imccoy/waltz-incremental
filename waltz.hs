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
-- How do you make two different actionreceivers using two different columns of a html table?


-- framework stuff
import Debug.Trace


-- framework: page display
follow_to source target = FuncAssociationWalk source target
showValueAsString = FuncF (\(ValueEntity entity) -> Text $ show entity)
showSectionText = FuncE getText
  where getText dta (ValueEntity entity) = case (entityValueItem dta entity) of
                                             EntitySection (Section text _) -> Text text
                                             e                              -> error $ "can't get text " ++ (show e)
        getText dta e = error $ "can't get text " ++ (show e)
showEntryText = FuncE getText
  where getText dta (ValueEntity entity) = case (entityValueItem dta entity) of
                                             EntityEntry (Entry text) -> Text text
                                             e                        -> error $ "can't get text " ++ (show e)
        getText dta e = error $ "can't get text " ++ (show e)


data Value = Tag String [(String, String)] Value
           | Text String
           | ValueEntity Entity
           | ValueFuncCall Func [Value]
           | Values [Value]
           | ActionReceiverValue ActionReceiver [Value]
           | ActionReceiverInput ActionReceiverField ActionReceiverControl
  deriving (Show)

data Func = FuncAssociationWalk String String | FuncEntityWhere (EntityValue -> Bool) | FuncMap (Value -> Value) | FuncF (Value -> Value) | FuncE (EntityData -> Value -> Value)

instance Show Func where
  show (FuncAssociationWalk source target) = "func: " ++ source ++ " -> " ++ target
  show _ = "func"

xmlencode [] = []
xmlencode ('"':xs) = "&quot;" ++ (xmlencode xs)
xmlencode ('<':xs) = "&lt;" ++ (xmlencode xs)
xmlencode ('>':xs) = "&gt;" ++ (xmlencode xs)
xmlencode (c:xs) = c:(xmlencode xs)

drawPage :: Value -> EntityData -> Action -> String
drawPage val dta causingAction = drawPageValue val 0
  where
    line indent string = concat $ (replicate indent "  ") ++ [string, "\n"]
    drawPageValue (Tag name attrs child) lvl = line lvl ("<" ++ name ++ (attrString attrs) ++  ">") ++ 
                                               drawPageValue child (lvl + 1) ++
                                               line lvl ("</" ++ name ++ ">")
      where attrString attrs = concat $ map (\(name, value) -> concat [" ", name, "=\"", (xmlencode value), "\""]) attrs
    drawPageValue (Text text) lvl = line lvl text
    drawPageValue (Values values) lvl = concat $ map (\v -> drawPageValue v lvl) values 
    drawPageValue func_call@(ValueFuncCall _ _) lvl = drawPageValue (eval func_call) lvl
    drawPageValue (ValueEntity entity) lvl = error ("can't draw value: entity " ++ (show entity))
    drawPageValue (ActionReceiverValue actionReceiver values) lvl = drawPageValue (Tag "form" [("method", "post")] $ Values (goingTo:comingFrom:values)) lvl
       where goingTo    = Tag "input" [("type", "hidden"), ("name", "actionReceiver"), ("value", actionReceiverName actionReceiver)] $ Values []
             comingFrom = Tag "input" [("type", "hidden"), ("name", "actionSource"), ("value", show causingAction)] $ Values []
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
        invoke' (FuncEntityWhere f) [] = ValueEntity $ entityWithValue dta f
        invoke' (FuncF f) [v] = f v
        invoke' (FuncF f) [] = error "No arguments to singular function"
        invoke' (FuncF f) _ = error "Too many arguments to singular function"
        invoke' (FuncE f) [v] = f dta v
        invoke' (FuncE f) [] = error "No arguments to singular entity function"
        invoke' (FuncE f) _ = error "Too many arguments to singular entity function"




-- framework: data store

data EntityData = EntityData Integer [EntityRelationship] [EntityValueItem]
emptyData = EntityData 0 [] []
data EntityRelationship = EntityRelationship Entity Entity
data EntityValueItem = EntityValueItem Entity EntityValue

data Entity = Entity Integer
  deriving (Show, Eq)

build_data :: EntityData -> [EntityData -> EntityData] -> EntityData
build_data d [] = d
build_data d (f:fs) = build_data (f d) fs

new_entity value (EntityData n rs vs) = (entity, EntityData (n+1) rs ((EntityValueItem entity value):vs))
  where entity = Entity $ n+1
add_entity parent child dta = let (entity, dta') = new_entity child dta
                               in case dta' of EntityData n rs vs -> EntityData n ((EntityRelationship parent entity):rs) vs

lookup_related :: EntityData -> Entity -> [Entity]
lookup_related (EntityData _ dta _) entity = map (\(EntityRelationship a b) -> b) $ (filter matchesEntity) dta
  where matchesEntity (EntityRelationship a b) = a == entity

entityValueItem :: EntityData -> Entity -> EntityValue
entityValueItem (EntityData _ _ dta) entity = (\(EntityValueItem a b) -> b) $ head $ (filter matchesEntity) dta
  where matchesEntity (EntityValueItem a _) = a == entity

entityWithValue :: EntityData -> (EntityValue -> Bool) -> Entity
entityWithValue (EntityData _ _ dta) matcher = case (filter matchesValue) dta of
                                                 ((EntityValueItem a b):_) -> a
                                                 otherwise -> error "no entity with matching value"
                                               where matchesValue (EntityValueItem _ v) = matcher v


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
                    let actions = actionsFor current_db action
                    let (response, new_db) = processActions action actions current_db
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
                                                last_action = read $ lastInQueryString request_body_query "actionSource"
                                                action_receiver = last $ filter (\h -> (actionReceiverName h) == action_receiver_name) allActionReceivers
                                                getter action_receiver_field = lastInQueryString request_body_query $ fieldName action_receiver_field
                                             in case validityErrors action_receiver getter of
                                                  []     -> (actionReceiverBuilder action_receiver) getter
                                                  errors -> (last_action `actionWith` action_receiver) -- include errors too, somehow

actionWith a receiver = a

validityErrors (ActionReceiver _ fields _) getter = errors fields
  where errors (field:fields) = fieldErrors field (validators field) $ errors fields
        errors [] = []
        fieldErrors field (v:vs) es = validate field v $ fieldErrors field vs es
        fieldErrors field [] es = es
        validate field (EnsureIsOneOf possibilities) errors = case filter ((==) $ getter field) possibilities of
                                                               []        -> (field,"not a valid possibility"):errors
                                                               otherwise -> errors
        validate field (EnsureIsNotEmpty) errors = case (getter field) == "" of
                                                     True  -> (field,"must not be empty"):errors
                                                     False -> errors

processActions initial_action actions current_db = processActions' actions current_db Nothing
  where
    processActions' ((AddPropertyOn entity property):actions) current_db page = processActions' actions (add_entity entity property current_db) page
    processActions' ((ShowPage page):actions) current_db Nothing = processActions' actions current_db (Just page)
    processActions' [] current_db (Just page) = (response, current_db)
      where response = responseLBS
                                status200
                                [("Content-Type", B8.pack "text/html")]
                                (LB8.pack $ drawPage (wrap_in_html $ page) current_db initial_action)

main = do
    putStrLn $ "http://localhost:8080/"
    db <- newIORef sample_data
    run 8080 $ app db

-- application stuff

data SectionMood = Good | Bad | Confusing
  deriving (Show, Read, Eq)
data Section = Section String SectionMood
  deriving (Show, Read, Eq)
data Entry = Entry String
  deriving (Show, Read, Eq)

data EntityValue = EntitySection Section | EntityEntry Entry
  deriving (Show, Read, Eq)

hasSectionMood desired_mood (EntitySection section@(Section _ mood)) = desired_mood == mood
hasSectionMood desired_mood section = False

data Action = AddPropertyOn Entity EntityValue | ShowPage Value | NewEntry SectionMood String | ShowRetro

readStringPair :: String -> (String, String)
readStringPair string = read string

instance Show Action where
  show (NewEntry section string) = show ("NewEntry", show (section, string))
  show (ShowRetro) = show ("ShowRetro", "")

instance Read Action where
  readsPrec _ string = let action = case readStringPair string of
                                      ("NewEntry", info)  -> let (section, string) = read info
                                                             in NewEntry section string
                                      ("ShowRetro", info) -> ShowRetro
                        in [(action, "")]


defaultAction = ShowRetro

actionsFor dta (ShowRetro) = [ShowPage $ retro_page]
actionsFor dta (NewEntry section text) = [AddPropertyOn (entityWithValue dta (hasSectionMood section)) (EntityEntry $ Entry text),
                                          (ShowPage retro_page) `actionWith` section]

possibleSectionStrings = ["Good", "Bad", "Confusing"]

-- this process has waaaaaay too much boilerplate
newEntryActionReceiverSectionMood = ActionReceiverField { fieldName = "sectionMood", validators = [EnsureIsOneOf possibleSectionStrings] }
newEntryActionReceiverText = ActionReceiverField { fieldName = "text", validators = [EnsureIsNotEmpty] }
newEntryActionReceiver = ActionReceiver "newEntry" [newEntryActionReceiverSectionMood, newEntryActionReceiverText] (\getter -> NewEntry (read $ getter (newEntryActionReceiverSectionMood)) (getter newEntryActionReceiverText))

allActionReceivers = [newEntryActionReceiver]

retro_entry entry = Tag "span" [] $ ValueFuncCall showEntryText [entry]

retro_entries section = Tag "div" [] $ Values [
                                      (Tag "h2" [] $ ValueFuncCall showSectionText [section]),
				      (ValueFuncCall (FuncMap retro_entry) [ValueFuncCall ("Section" `follow_to` "Entry") [section]])]

retro_view = Tag "div" [] $ Values [retro_entries (ValueFuncCall (FuncEntityWhere $ hasSectionMood Good) []),
                                    retro_entries (ValueFuncCall (FuncEntityWhere $ hasSectionMood Bad) []),
                                    retro_entries (ValueFuncCall (FuncEntityWhere $ hasSectionMood Confusing) [])]
           
sample_data = let (good, dta) = new_entity (EntitySection $ Section "Good" Good) emptyData
                  (bad, dta') = new_entity (EntitySection $ Section "Bad" Bad) dta
                  (confusing, dta'') = new_entity (EntitySection $ Section "Ugly" Confusing) dta'
               in build_data dta''
                     [
                      add_entity good (EntityEntry $ Entry "It's Okay"),
                      add_entity bad (EntityEntry $ Entry "It's Ugly")
                      ]

retro_page = Values [(Tag "h1" [] $ Text "Retro"),
                                     retro_view,
                                     ActionReceiverValue newEntryActionReceiver [
                                       ActionReceiverInput newEntryActionReceiverSectionMood $ Dropdown possibleSectionStrings,
                                       ActionReceiverInput newEntryActionReceiverText Textfield
                                     ]]

wrap_in_html body = Tag "html" [] $ Values [
                                    (Tag "head" [] $ Tag "title" [] $ Text "Waltz App"),
                                    (Tag "body" [] body)]


