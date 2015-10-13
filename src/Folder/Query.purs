module Folder.Query where

import Data.Maybe (Maybe())

data FolderQuery a = CreateList a
                   | SetNextListTitle String a
                   | ChooseList (Maybe Int) a
