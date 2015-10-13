module Folder.List.Query where

import Folder.List.Model (List())

data ListQuery a = CreateItem a
                 | RemoveList a
