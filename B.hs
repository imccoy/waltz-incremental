{-# LANGUAGE OverloadedStrings #-}
module B where

import Waltz

import Control.Monad (forM_)

import Text.Blaze.Html5 hiding (head, map)
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


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
  where sectionWith mood = let section_actions = filter (\entryAction -> entryActionSection entryAction == mood) actions
                               deleted_idents = map deleteEntryEntryIdent $ filter isDeleteEntry section_actions
                               notDeleted x = all (/= newEntryEntryIdent x) deleted_idents
                               entries = [ Entry { entryText = newEntryText entryAction, entryIdent = newEntryEntryIdent entryAction }
                                         | entryAction <- filter notDeleted $ filter isNewEntry section_actions]
                            in Section { sectionName = show mood, sectionMood = mood, sectionEntries = entries }

sectionWithMood mood retro_state = head $ filter (\section -> sectionMood section == mood) retro_state

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

main = runApp sample_data applicationState retro_page allActionReceivers
