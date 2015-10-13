module Folder.List.Item.Model where

type Item = { id :: Int, checked :: Boolean, text :: String }

initialItem :: Int -> Item
initialItem id = { id: id, checked: false, text: "" }
