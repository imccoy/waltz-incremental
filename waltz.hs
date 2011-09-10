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

data Effect = YieldEffect Action | ShowPage (ApplicationState -> Action -> Html)
data ActionResult = MoreActions [Action] | ActionEffect Effect

effectsFor dta action = effectsFor' dta [action] []
  where effectsFor' dta [] effects = effects
        effectsFor' dta (a:as) effects =  case actionResult dta a of
                           MoreActions actions -> effectsFor' dta (as ++ actions) effects
                           ActionEffect effect -> effectsFor' dta as (effect:effects)

-- framework: page display


actionReceiverHtml actionReceiver causingAction children = H.form ! method "post" $ do
                                                             input ! type_ "hidden" ! name "actionReceiver" ! value (toValue $ actionReceiverName actionReceiver)
                                                             input ! type_ "hidden" ! name "actionSource" ! value (toValue $ show causingAction)
                                                             children

actionReceiverInput actionReceiverField Textfield = input ! name (toValue $ fieldName actionReceiverField)
actionReceiverInput actionReceiverField (Dropdown options) = select ! name (toValue $ fieldName actionReceiverField) $ do
                                                               forM_ options (\optionText -> option $ toHtml optionText)
actionReceiverInput actionReceiverField (Hidden v) = input ! type_ "hidden" ! name (toValue $ fieldName actionReceiverField) ! value (toValue v)

actionReceiverSend = input ! type_ "submit"

-- framework: user input

data Ensurer = EnsureIsOneOf [String] | EnsureIsNotEmpty | EnsureIsIntegral
  deriving (Show)

data ActionReceiverField = ActionReceiverField { fieldName :: String, validators :: [Ensurer] }
  deriving (Show)

data ActionReceiver = ActionReceiver String [ActionReceiverField] ((ActionReceiverField -> String) -> Int -> Action)
actionReceiverName (ActionReceiver name _ _) = name
actionReceiverBuilder (ActionReceiver _ _ builder) = builder

data ActionReceiverControl = Textfield | Dropdown [String] | Hidden String
  deriving (Show)

instance Show ActionReceiver where
  show (ActionReceiver name fields func) = "ActionReceiver " ++ name ++ " " ++ (show fields)

-- http

app :: IORef [Action] -> Application 
app db request = do current_db <- tryIO $ readIORef db
                    request_body_chunks <- EL.consume
                    let request_body_query = parseQuery $ B.concat request_body_chunks
                    let (last_action, action) = processRequest request current_db request_body_query
                    let effects = effectsFor current_db action
                    let (response, new_db) = processEffects action effects current_db last_action
                    tryIO $ writeIORef db new_db
                    return response

processRequest request current_db request_body_query = case parseMethod $ requestMethod request of
                                                         (Right POST) ->  actionReceiverToAction request_body_query (length current_db)
                                                         otherwise -> (defaultAction, defaultAction) -- TODO: better cope with these cases: no action to take, take the last action but augment with error information, take the last action but pre-fill some form stuff

lastInQueryString :: Query -> String -> String
lastInQueryString query name = case filter (\(n, v) -> n == (B8.pack name)) $ query of
                                 []             -> error "nothing for " ++ name ++ " in " ++ (show query)
                                 lookup_matches -> B8.unpack $ fromJust $ snd $ last $ lookup_matches

actionReceiverWithName name = case filter (\h -> (actionReceiverName h) == name) allActionReceivers of
                                []        -> error $ "no receiver with name " ++ name
                                receivers -> last receivers

actionReceiverToAction request_body_query n = let action_receiver_name = lastInQueryString request_body_query "actionReceiver"
                                                  last_action = read $ lastInQueryString request_body_query "actionSource"
                                                  action_receiver = actionReceiverWithName action_receiver_name
                                                  field_getter action_receiver_field = lastInQueryString request_body_query $ fieldName action_receiver_field
                                               in case validityErrors action_receiver field_getter of
                                                    []     -> (last_action, (actionReceiverBuilder action_receiver) field_getter n)
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
        validate field (EnsureIsIntegral) errors = case all (\c -> elem c "0123456789") $ getter field of
                                                     False -> (field,"must be integral"):errors
                                                     True  -> errors

processEffects initial_action effects current_db last_action = processEffects' effects current_db Nothing
  where
    processEffects' ((YieldEffect action):effects) current_db page = processEffects' effects (current_db ++ [action]) page
    processEffects' ((ShowPage page):effects) current_db Nothing = processEffects' effects current_db (Just page)
    processEffects' [] current_db Nothing = processEffects' [] current_db (Just $ showPageFromEffects $ effectsFor current_db last_action)
      where showPageFromEffects [] = error ("no show page effect for " ++ (show last_action))
            showPageFromEffects ((ShowPage v):_) = v
            showPageFromEffects (_:effects) = showPageFromEffects effects
    processEffects' [] current_db (Just page) = (response, current_db)
      where response = responseLBS
                                status200
                                [("Content-Type", B8.pack "text/html")]
                                (renderHtml (wrap_in_html $ page (applicationState current_db) last_action))

main = do
    putStrLn $ "http://localhost:8080/"
    db <- newIORef sample_data
    run 8080 $ app db

-- application stuff

data SectionMood = Good | Bad | Confusing
  deriving (Show, Read, Eq)
data Section = Section { sectionName :: String, sectionMood :: SectionMood, sectionEntries :: [Entry] }
  deriving (Show, Read, Eq)
data Entry = Entry { entryText :: String, entryIdent :: Ident }
  deriving (Show, Read, Eq)

type Ident = Int

data Action = NewEntry { newEntrySection :: SectionMood, newEntryText :: String, newEntryEntryIdent :: Ident } 
            | DeleteEntry { deleteEntrySection :: SectionMood,  deleteEntryEntryIdent :: Ident, deleteEntryActionIdent :: Ident }
            | ShowRetro
  deriving (Show, Read)

isNewEntry (NewEntry _ _ _) = True
isNewEntry _                = False
isDeleteEntry (DeleteEntry _ _ _) = True
isDeleteEntry _                   = False
entryActionSection a
  | isNewEntry a = newEntrySection a
  | isDeleteEntry a = deleteEntrySection a

data RetroState = RetroState { stateSections :: [Section]  }
type ApplicationState = RetroState
applicationState actions = RetroState { stateSections = [sectionWith Good, sectionWith Bad, sectionWith Confusing] }
  where sectionWith sectionMood = let sectionActions = filter (\entryAction -> entryActionSection entryAction == sectionMood) actions
                                      deleted_idents = map deleteEntryEntryIdent $ filter isDeleteEntry sectionActions
                                      notDeleted x = all (/= newEntryEntryIdent x) deleted_idents
                                      entries = [ Entry { entryText = newEntryText entryAction, entryIdent = newEntryEntryIdent entryAction }
                                                   | entryAction <- filter notDeleted $ filter isNewEntry sectionActions]
                                   in Section { sectionName = show sectionMood, sectionMood = sectionMood, sectionEntries = entries }

sectionWithMood mood retro_state = head $ filter (\section -> sectionMood section == mood) retro_state

defaultAction = ShowRetro

actionResult dta (ShowRetro) = ActionEffect $ ShowPage retro_page
actionResult dta action@(NewEntry section text ident) = ActionEffect $ YieldEffect action
actionResult dta action@(DeleteEntry section entryIdent ident) = ActionEffect $ YieldEffect action

possibleSectionStrings = ["Good", "Bad", "Confusing"]

-- this process has waaaaaay too much boilerplate
actionReceiverSectionMood = ActionReceiverField { fieldName = "sectionMood", validators = [EnsureIsOneOf possibleSectionStrings] }
newEntryActionReceiverSectionMood = actionReceiverSectionMood
newEntryActionReceiverText = ActionReceiverField { fieldName = "text", validators = [EnsureIsNotEmpty] }
newEntryActionReceiver = ActionReceiver "newEntry" [newEntryActionReceiverSectionMood, newEntryActionReceiverText] (\getter -> NewEntry (read $ getter (newEntryActionReceiverSectionMood)) (getter newEntryActionReceiverText))

deleteEntryActionReceiverSectionMood = actionReceiverSectionMood
deleteEntryActionReceiverEntryIdent = ActionReceiverField { fieldName = "entryId", validators = [EnsureIsIntegral] }
deleteEntryActionReceiver = ActionReceiver "deleteEntry" [deleteEntryActionReceiverSectionMood, deleteEntryActionReceiverEntryIdent] (\getter -> DeleteEntry (read $ getter deleteEntryActionReceiverSectionMood) (read $ getter deleteEntryActionReceiverEntryIdent))

allActionReceivers = [newEntryActionReceiver, deleteEntryActionReceiver]

retro_entry entry section causing_action = H.span $ do
                                             toHtml $ entryText entry
                                             actionReceiverHtml deleteEntryActionReceiver causing_action $ do
                                               actionReceiverInput deleteEntryActionReceiverSectionMood $ Hidden $ show $ sectionMood section
                                               actionReceiverInput deleteEntryActionReceiverEntryIdent $ Hidden $ show $ entryIdent entry
                                               actionReceiverSend

retro_entries section causing_action = H.div $ do
                                         h2 $ toHtml $ sectionName section
                                         forM_ (sectionEntries section) $ (\e -> retro_entry e section causing_action)

retro_view retro_state causing_action = H.div $ do
                                          retro_entries (sectionWithMood Good retro_state) causing_action
                                          retro_entries (sectionWithMood Bad retro_state) causing_action
                                          retro_entries (sectionWithMood Confusing retro_state) causing_action
           
sample_data = [NewEntry Good "It's Okay" 0,
               NewEntry Bad "It's Ugly" 1]

retro_page retro_state causing_action = H.div $ do
                                          h1 "Retro"
                                          retro_view (stateSections retro_state) causing_action
                                          actionReceiverHtml newEntryActionReceiver causing_action $ do
                                            actionReceiverInput newEntryActionReceiverSectionMood $ Dropdown possibleSectionStrings
                                            actionReceiverInput newEntryActionReceiverText Textfield

wrap_in_html body = docTypeHtml $ do 
                      H.head $ do
                        H.title "Waltz App"
                      H.body body


