-- TODOS
-- "Value" should not construct a HtmlElement
-- Should HtmlElement and Value both come in plural varieties?
-- is a function to a Value different to a function to a HtmlElement?
-- should Values contain HtmlElements *and* vice versa?


-- framework stuff
import Debug.Trace


follow_to source target = FuncAssociationWalk source target
showValueAsString = FuncF (\(ValueEntity entity) -> ValueHtmlElement $ Text $ show entity)
showEntryText = FuncF getText
  where getText (ValueEntity (EntityEntry (Entry text))) = ValueHtmlElement $ Text text
        getText e = error $ "can't get text " ++ (show e)


data Value = ValueHtmlElement HtmlElement | ValueHtmlElements HtmlElements | ValueEntity Entity | ValueFuncCall FuncCall | Values [Value]
  deriving (Show)

data Func = FuncAssociationWalk String String | FuncMap (Value -> Value) | FuncF (Value -> Value)
data FuncCall = FuncCall Func [Value]

instance Show FuncCall where
  show (FuncCall (FuncAssociationWalk source target) _) = "function call: " ++ source ++ " -> " ++ target
  show _ = "function call"

data HtmlElements = HtmlElements [HtmlElement] | HtmlElementsEmpty | HtmlElementsFuncCall FuncCall | HtmlElementsConcat [HtmlElements]
  deriving (Show)
data HtmlElement = Tag String HtmlElements | Value Value | Text String | HtmlElementFuncCall FuncCall
  deriving (Show)

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

-- application stuff

data Section = Good | Bad | Confusing
  deriving (Show, Eq)
data Entry = Entry String
  deriving (Show, Eq)

data Entity = EntitySection Section | EntityEntry Entry
  deriving (Show, Eq)

drawPage :: HtmlElement -> EntityData -> String
drawPage elt dta = drawPageElement elt 0
  where
    line indent string = concat $ (replicate indent "  ") ++ [string, "\n"]
    drawPageElement (Tag name children) lvl = line lvl ("<" ++ name ++ ">") ++ 
                                                drawPageElements children (lvl + 1) ++
                                                line lvl ("</" ++ name ++ ">")
    drawPageElement (HtmlElementFuncCall func_call) lvl = drawPageFuncCall func_call lvl
    drawPageElement (Text text) lvl = line lvl text
    drawPageElement _ lvl = error "unimplemented page element type"

    drawPageElements HtmlElementsEmpty lvl = ""
    drawPageElements (HtmlElements []) lvl = ""
    drawPageElements (HtmlElements (elt:elts)) lvl = drawPageElement elt lvl ++ (drawPageElements (HtmlElements elts) lvl)
    drawPageElements (HtmlElementsConcat []) lvl = ""
    drawPageElements (HtmlElementsConcat (elts:eltss)) lvl = drawPageElements elts lvl ++ (drawPageElements (HtmlElementsConcat eltss) lvl)
    drawPageElements (HtmlElementsFuncCall func_call) lvl = drawPageFuncCall func_call lvl

    drawPageFuncCall func_call lvl = drawPageValue (eval func_call) lvl

    drawPageValue (ValueHtmlElement htmlElement) lvl = drawPageElement htmlElement lvl
    drawPageValue (ValueHtmlElements htmlElements) lvl = drawPageElements htmlElements lvl
    drawPageValue (Values values) lvl = concat $ map (\v -> drawPageValue v lvl) values 
    drawPageValue (ValueEntity entity) lvl = error ("can't draw value: entity " ++ (show entity))

    eval :: FuncCall -> Value
    eval (FuncCall func args) = invoke func args

    eval' :: Value -> Value
    eval' (ValueFuncCall func_call) = eval func_call
    eval' v = v

    invoke :: Func -> [Value] -> Value
    invoke func args = invoke' func (map eval' args)
      where
        invoke' (FuncAssociationWalk source_type attr) [ValueEntity entity] = Values $ map ValueEntity $ lookup_related dta entity
        invoke' (FuncAssociationWalk source_type attr) vs = error $ "Can't walk association over " ++ (show vs)
        invoke' (FuncMap f) [Values vs] = Values $ map f vs
        invoke' (FuncMap f) vs = Values $ map f vs
        invoke' (FuncF f) [v] = f v
        invoke' (FuncF f) [] = error "No arguments to singular function"
        invoke' (FuncF f) _ = error "Too many arguments to singular function"


lookup_related :: [EntityStore] -> Entity -> [Entity]
lookup_related dta entity = map (\(EntityStoreEntry a b) -> b) $ (filter matchesEntity) dta
  where matchesEntity :: EntityStore -> Bool
        matchesEntity (EntityStoreEntry a b) = a == entity


entityLand = 
	haveEntity "Section" $
        haveEntity "Entry" $
        haveEntity "Text" $ 
       	"Section" `relates_to_multiple` "Entry" $
        "Entry" `relates_to_one` "Text" $
        emptyEntityLand


retro_entry entry = ValueHtmlElement $ Tag "span" $ HtmlElements [HtmlElementFuncCall $ FuncCall showEntryText [entry]]

retro_entries section = Tag "div" $ HtmlElementsConcat [
                                      (HtmlElements [Tag "h1" $ HtmlElements [HtmlElementFuncCall $ FuncCall showValueAsString [section]]]),
				      (HtmlElementsFuncCall $ FuncCall (FuncMap retro_entry) [ValueFuncCall $ FuncCall ("Section" `follow_to` "Entry") [section]])]

retro = Tag "div" $ HtmlElements [retro_entries(ValueEntity $ EntitySection Good),
                                  retro_entries(ValueEntity $ EntitySection Bad),
                                  retro_entries(ValueEntity $ EntitySection Confusing)]
           
sample_data = build_data entityLand
                     [
                      add_entity (EntitySection Good) (EntityEntry $ Entry "It's Okay"),
                      add_entity (EntitySection Bad) (EntityEntry $ Entry "It's Ugly")
                      ]


main = putStrLn $ drawPage retro sample_data
