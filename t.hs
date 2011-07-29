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
           | HapperReceiver Happer [Value]
           | HapperInput HapperField HapperControl
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
    drawPageValue (HapperReceiver happer values) lvl = drawPageValue (Tag "form" [("method", "post")] $ Values (input:values)) lvl
       where input = Tag "input" [("type", "hidden"), ("name", "happer"), ("value", happerName happer)] $ Values []
    drawPageValue (HapperInput happerField Textfield) lvl = drawPageValue (Tag "input" [("name", fieldName happerField)] $ Values []) lvl
    drawPageValue (HapperInput happerField (Dropdown options)) lvl = drawPageValue (Tag "select" [("name", fieldName happerField)] $ Values optionTags) lvl
       where optionTags = map (\option -> Tag "option" [] $ Text option) options

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




-- framework: data schema

data EntityLand = EntityLand { entityNames :: [String]
                             , entitiesRelatingToMultiple :: [(String, String)]
                             , entitiesRelatingToOne :: [(String, String)] }

emptyEntityLand = EntityLand { entityNames = [], entitiesRelatingToMultiple = [], entitiesRelatingToOne = [] }


addEntity name entityLand = entityLand { entityNames = (name:(entityNames entityLand)) }
addEntitiesRelatingToMultiple names entityLand = entityLand { entitiesRelatingToMultiple = (names:(entitiesRelatingToMultiple entityLand)) }
addEntitiesRelatingToOne names entityLand = entityLand { entitiesRelatingToOne = (names:(entitiesRelatingToOne entityLand)) }

haveEntity = addEntity
relates_to_multiple a b = addEntitiesRelatingToMultiple (a,b)
relates_to_one a b = addEntitiesRelatingToOne (a,b)

-- framework: data store

type EntityData = [EntityStore]
emptyData = []
data EntityStore = EntityStoreEntry Entity Entity

build_data :: EntityLand -> [EntityLand -> EntityData -> EntityData] -> EntityData
build_data el fs = build_data' el emptyData fs
  where build_data' el d [] = d
        build_data' el d (f:fs) = build_data' el (f el d) fs

add_entity parent child el d = (EntityStoreEntry parent child):d
add_entity_in _ parent child el d = (EntityStoreEntry parent child):d
add_property_on parent _ child el d = (EntityStoreEntry parent child):d

lookup_related :: [EntityStore] -> Entity -> [Entity]
lookup_related dta entity = map (\(EntityStoreEntry a b) -> b) $ (filter matchesEntity) dta
  where matchesEntity :: EntityStore -> Bool
        matchesEntity (EntityStoreEntry a b) = a == entity

data Action = AddPropertyOn Entity Entity

-- framework: user input

data Ensurer = EnsureIsOneOf [String] | EnsureIsNotEmpty
  deriving (Show)

data HapperField = HapperField { fieldName :: String, validators :: [Ensurer] }
  deriving (Show)

data Happer = Happer String [HapperField] ((HapperField -> String) -> Hap)
happerName (Happer name _ _) = name
happerBuilder (Happer _ _ builder) = builder

data HapperControl = Textfield | Dropdown [String]
  deriving (Show)

instance Show Happer where
  show (Happer name fields func) = "Happer " ++ name ++ " " ++ (show fields)

-- http

app :: IORef EntityData -> Application 
app db request = do current_db <- tryIO $ readIORef db
                    request_body_chunks <- EL.consume
                    let request_body_query = parseQuery $ B.concat request_body_chunks
                    let hap = processRequest request current_db request_body_query
                    let actions = actionsForHap hap
                    let (response, new_db) = processActions actions current_db
                    tryIO $ writeIORef db new_db
                    return response

processRequest request current_db request_body_query = case parseMethod $ requestMethod request of
                                                         (Right POST) ->  happerToHap request_body_query
                                                         otherwise -> defaultHap

lastInQueryString :: Query -> String -> String
lastInQueryString query name = case filter (\(n, v) -> n == (B8.pack name)) $ query of
                                 []             -> error "nothing for " ++ name ++ " in " ++ (show query)
                                 lookup_matches -> B8.unpack $ fromJust $ snd $ last $ lookup_matches

happerToHap request_body_query = let happer_name = lastInQueryString request_body_query "happer"
                                     happer = last $ filter (\h -> (happerName h) == happer_name) allHappers
                                     getter happerField = lastInQueryString request_body_query $ fieldName happerField
                                  in (happerBuilder happer) getter

processActions ((AddPropertyOn entity property):actions) current_db = processActions actions (add_property_on entity entity property entityLand current_db)
processActions [] current_db = (response, current_db)
  where response = responseLBS
                            status200
                            [("Content-Type", B8.pack "text/html")]
                            (LB8.pack $ drawPage (wrap_in_html retro_page) current_db)

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

data Hap = NewEntry Section String | ShowRetro

defaultHap = ShowRetro

actionsForHap ShowRetro = []
actionsForHap (NewEntry section text) = [AddPropertyOn (EntitySection section) (EntityEntry $ Entry text)]

entityLand = 
	haveEntity "Section" $
        haveEntity "Entry" $
        haveEntity "Text" $ 
       	"Section" `relates_to_multiple` "Entry" $
        "Entry" `relates_to_one` "Text" $
        emptyEntityLand

possibleSectionStrings = ["Good", "Bad", "Confusing"]

newEntryHapperSection = HapperField { fieldName = "section", validators = [EnsureIsOneOf possibleSectionStrings] }
newEntryHapperText = HapperField { fieldName = "text", validators = [EnsureIsNotEmpty] }
newEntryHapper = Happer "newEntry" [newEntryHapperSection, newEntryHapperText] (\getter -> NewEntry (read $ getter (newEntryHapperSection)) (getter newEntryHapperText))

allHappers = [newEntryHapper]


retro_entry entry = Tag "span" [] $ ValueFuncCall showEntryText [entry]

retro_entries section = Tag "div" [] $ Values [
                                      (Tag "h2" [] $ ValueFuncCall showValueAsString [section]),
				      (ValueFuncCall (FuncMap retro_entry) [ValueFuncCall ("Section" `follow_to` "Entry") [section]])]

retro = Tag "div" [] $ Values [retro_entries(ValueEntity $ EntitySection Good),
                            retro_entries(ValueEntity $ EntitySection Bad),
                            retro_entries(ValueEntity $ EntitySection Confusing)]
           
sample_data = build_data entityLand
                     [
                      add_entity (EntitySection Good) (EntityEntry $ Entry "It's Okay"),
                      add_entity (EntitySection Bad) (EntityEntry $ Entry "It's Ugly")
                      ]

retro_page = Values [(Tag "h1" [] $ Text "Retro"),
                     retro,
                     HapperReceiver newEntryHapper [
                       HapperInput newEntryHapperSection (Dropdown possibleSectionStrings),
                       HapperInput newEntryHapperText Textfield
                     ]]

wrap_in_html body = Tag "html" [] $ Values [
                                    (Tag "head" [] $ Tag "title" [] $ Text "Waltz App"),
                                    (Tag "body" [] body)]


