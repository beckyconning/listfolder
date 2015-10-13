module Folder.Model where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array (filter, snoc)

import Folder.List.Model (List())

type Folder = { nextListTitle :: String
              , lists :: Array { title :: String, listId :: Int }
              , currentListId :: Maybe Int
              , lastListId :: Int
              }

initialFolder :: Folder
initialFolder = { nextListTitle: "", lists: [], currentListId: Nothing, lastListId: 0 }

createList :: Folder -> Folder
createList folder =
  folder { lists = folder.lists `snoc` { title: folder.nextListTitle, listId: listId }
         , nextListTitle = ""
         , lastListId = listId
         }
           where
           listId = folder.lastListId + 1

removeList :: Int -> Folder -> Folder
removeList listId folder = folder { lists = filter (\item -> item.listId /= listId) folder.lists }
