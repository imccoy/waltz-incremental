{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (forM_)

import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B

import Data.Enumerator (tryIO)
import qualified Data.Enumerator.List as EL

import Data.IORef
import Data.Maybe (fromJust)

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import Text.Blaze.Html5 hiding (head, map)
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8 (renderHtml)

-- TODOS
-- How do you make two different actionreceivers using two different columns of a html table?


-- framework stuff
import Debug.Trace

data Effect = AddPropertyOn Entity EntityValue | ShowPage (EntityData -> Action -> Html)
data ActionResult = MoreActions [Action] | ActionEffect Effect

effectsFor dta action = effectsFor' dta [action] []
  where effectsFor' dta [] effects = effects
        effectsFor' dta (a:as) effects =  case actionResult dta a of
                           MoreActions actions -> effectsFor' dta (as ++ actions) effects
                           ActionEffect effect -> effectsFor' dta as (effect:effects)

-- framework: page display
showSectionText dta ent = toHtml $ getText dta ent
  where getText dta entity = case (entityValueItem dta entity) of
                                             EntitySection (Section text _) -> text
                                             e                              -> error $ "can't get text " ++ (show e)
showEntryText dta ent = toHtml $ getText dta ent
  where getText dta entity = case (entityValueItem dta entity) of
                                             EntityEntry (Entry text) -> text
                                             e                        -> error $ "can't get text " ++ (show e)


actionReceiverHtml actionReceiver causingAction children = H.form ! method "post" $ do
                                                             input ! type_ "hidden" ! name "actionReceiver" ! value (toValue $ actionReceiverName actionReceiver)
                                                             input ! type_ "hidden" ! name "actionSource" ! value (toValue $ show causingAction)
                                                             children

actionReceiverInput actionReceiverField Textfield = input ! name (toValue $ fieldName actionReceiverField)
actionReceiverInput actionReceiverField (Dropdown options) = select ! name (toValue $ fieldName actionReceiverField) $ do
                                                               forM_ options (\optionText -> option $ toHtml optionText)

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
                    let (last_action, action) = processRequest request current_db request_body_query
                    let effects = effectsFor current_db action
                    let (response, new_db) = processEffects action effects current_db last_action
                    tryIO $ writeIORef db new_db
                    return response

processRequest request current_db request_body_query = case parseMethod $ requestMethod request of
                                                         (Right POST) ->  actionReceiverToAction request_body_query
                                                         otherwise -> (defaultAction, defaultAction) -- TODO: better cope with these cases: no action to take, take the last action but augment with error information, take the last action but pre-fill some form stuff

lastInQueryString :: Query -> String -> String
lastInQueryString query name = case filter (\(n, v) -> n == (B8.pack name)) $ query of
                                 []             -> error "nothing for " ++ name ++ " in " ++ (show query)
                                 lookup_matches -> B8.unpack $ fromJust $ snd $ last $ lookup_matches

actionReceiverToAction request_body_query = let action_receiver_name = lastInQueryString request_body_query "actionReceiver"
                                                last_action = read $ lastInQueryString request_body_query "actionSource"
                                                action_receiver = last $ filter (\h -> (actionReceiverName h) == action_receiver_name) allActionReceivers
                                                getter action_receiver_field = lastInQueryString request_body_query $ fieldName action_receiver_field
                                             in case validityErrors action_receiver getter of
                                                  []     -> (last_action, (actionReceiverBuilder action_receiver) getter)
                                                  errors -> (last_action, last_action `actionWith` action_receiver) -- include errors too, somehow

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

processEffects initial_action effects current_db last_action = processEffects' effects current_db Nothing
  where
    processEffects' ((AddPropertyOn entity property):effects) current_db page = processEffects' effects (add_entity entity property current_db) page
    processEffects' ((ShowPage page):effects) current_db Nothing = processEffects' effects current_db (Just page)
    processEffects' [] current_db Nothing = processEffects' [] current_db (Just $ showPageFromEffects $ effectsFor current_db last_action)
      where showPageFromEffects [] = error ("no show page effect for " ++ (show last_action))
            showPageFromEffects ((ShowPage v):_) = v
            showPageFromEffects (_:effects) = showPageFromEffects effects
    processEffects' [] current_db (Just page) = (response, current_db)
      where response = responseLBS
                                status200
                                [("Content-Type", B8.pack "text/html")]
                                (renderHtml (wrap_in_html $ page current_db last_action))

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

data Action = NewEntry SectionMood String | ShowRetro
  deriving (Show, Read)

readStringPair :: String -> (String, String)
readStringPair string = read string

defaultAction = ShowRetro

actionResult dta (ShowRetro) = ActionEffect $ ShowPage retro_page
actionResult dta (NewEntry section text) = ActionEffect $ AddPropertyOn (entityWithValue dta (hasSectionMood section)) (EntityEntry $ Entry text)

possibleSectionStrings = ["Good", "Bad", "Confusing"]

-- this process has waaaaaay too much boilerplate
newEntryActionReceiverSectionMood = ActionReceiverField { fieldName = "sectionMood", validators = [EnsureIsOneOf possibleSectionStrings] }
newEntryActionReceiverText = ActionReceiverField { fieldName = "text", validators = [EnsureIsNotEmpty] }
newEntryActionReceiver = ActionReceiver "newEntry" [newEntryActionReceiverSectionMood, newEntryActionReceiverText] (\getter -> NewEntry (read $ getter (newEntryActionReceiverSectionMood)) (getter newEntryActionReceiverText))

allActionReceivers = [newEntryActionReceiver]

retro_entry dta entry = H.span $ showEntryText dta entry

retro_entries dta section = H.div $ do
                              h2 $ showSectionText dta section
                              let entries = lookup_related dta section
                              forM_ entries (retro_entry dta)

retro_view dta = H.div $ do
                   retro_entries dta $ entityWithValue dta (hasSectionMood Good)
                   retro_entries dta $ entityWithValue dta $ hasSectionMood Bad
                   retro_entries dta $ entityWithValue dta $ hasSectionMood Confusing
           
sample_data = let (good, dta) = new_entity (EntitySection $ Section "Good" Good) emptyData
                  (bad, dta') = new_entity (EntitySection $ Section "Bad" Bad) dta
                  (confusing, dta'') = new_entity (EntitySection $ Section "Ugly" Confusing) dta'
               in build_data dta''
                     [
                      add_entity good (EntityEntry $ Entry "It's Okay"),
                      add_entity bad (EntityEntry $ Entry "It's Ugly")
                      ]

retro_page dta causingAction = H.div $ do
                                 h1 "Retro"
                                 retro_view dta
                                 actionReceiverHtml newEntryActionReceiver causingAction $ do
                                   actionReceiverInput newEntryActionReceiverSectionMood $ Dropdown possibleSectionStrings
                                   actionReceiverInput newEntryActionReceiverText Textfield

wrap_in_html body = docTypeHtml $ do 
                      H.head $ do
                        H.title "Waltz App"
                      H.body body


