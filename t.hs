-- TODOS
-- "Value" should not construct a HtmlElement
-- Should HtmlElement and Value both come in plural varieties?
-- is a function to a Value different to a function to a HtmlElement?
-- should Values contain HtmlElements *and* vice versa?


-- framework stuff
import Debug.Trace


-- framework: page display
follow_to source target = FuncAssociationWalk source target
showValueAsString = FuncF (\(ValueEntity entity) -> Text $ show entity)
showEntryText = FuncF getText
  where getText (ValueEntity (EntityEntry (Entry text))) = Text text
        getText e = error $ "can't get text " ++ (show e)


data Value = Tag String Value | Text String | ValueEntity Entity | ValueFuncCall Func [Value] | Values [Value]
  deriving (Show)

data Func = FuncAssociationWalk String String | FuncMap (Value -> Value) | FuncF (Value -> Value)

instance Show Func where
  show (FuncAssociationWalk source target) = "func: " ++ source ++ " -> " ++ target
  show _ = "func"


drawPage :: Value -> EntityData -> String
drawPage val dta = drawPageValue val 0
  where
    line indent string = concat $ (replicate indent "  ") ++ [string, "\n"]
    drawPageValue (Tag name child) lvl = line lvl ("<" ++ name ++ ">") ++ 
                                         drawPageValue child (lvl + 1) ++
                                         line lvl ("</" ++ name ++ ">")
    drawPageValue (Text text) lvl = line lvl text
    drawPageValue (Values values) lvl = concat $ map (\v -> drawPageValue v lvl) values 
    drawPageValue func_call@(ValueFuncCall _ _) lvl = drawPageValue (eval func_call) lvl
    drawPageValue (ValueEntity entity) lvl = error ("can't draw value: entity " ++ (show entity))

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


-- application stuff

data Section = Good | Bad | Confusing
  deriving (Show, Eq)
data Entry = Entry String
  deriving (Show, Eq)

data Entity = EntitySection Section | EntityEntry Entry
  deriving (Show, Eq)

entityLand = 
	haveEntity "Section" $
        haveEntity "Entry" $
        haveEntity "Text" $ 
       	"Section" `relates_to_multiple` "Entry" $
        "Entry" `relates_to_one` "Text" $
        emptyEntityLand


retro_entry entry = Tag "span" $ ValueFuncCall showEntryText [entry]

retro_entries section = Tag "div" $ Values [
                                      (Tag "h2" $ ValueFuncCall showValueAsString [section]),
				      (ValueFuncCall (FuncMap retro_entry) [ValueFuncCall ("Section" `follow_to` "Entry") [section]])]

retro = Tag "div" $ Values [retro_entries(ValueEntity $ EntitySection Good),
                            retro_entries(ValueEntity $ EntitySection Bad),
                            retro_entries(ValueEntity $ EntitySection Confusing)]
           
sample_data = build_data entityLand
                     [
                      add_entity (EntitySection Good) (EntityEntry $ Entry "It's Okay"),
                      add_entity (EntitySection Bad) (EntityEntry $ Entry "It's Ugly")
                      ]

retro_page = Values [(Tag "h1" $ Text "Retro"),
                     retro]

wrap_in_html body = Tag "html" $ Values [
                                    (Tag "head" $ Tag "title" $ Text "Waltz App"),
                                    (Tag "body" body)]

main = putStrLn $ drawPage (wrap_in_html retro_page) sample_data
