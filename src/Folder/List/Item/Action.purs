module Folder.List.Item.Query where

import Folder.List.Item.Model (Item())

data ItemQuery a = SetChecked Boolean a
                 | SetText String a
                 | RemoveItem a
                 | GetItem (Item -> a)
