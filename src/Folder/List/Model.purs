module Folder.List.Model where

import Prelude

import Data.Array (snoc, filter)

import Folder.List.Item.Model (Item())

type List = { id :: Int, title :: String, items :: Array Int, lastItemId :: Int }

initialList :: Int -> String -> List
initialList id title = { id: id, title: title, items: [], lastItemId: 0 }

createItem :: List -> List
createItem list = list { items = list.items `snoc` itemId, lastItemId = itemId }
  where
  itemId = list.lastItemId + 1

removeItem :: Int -> List -> List
removeItem itemId list = list { items = filter (/= itemId) list.items }
