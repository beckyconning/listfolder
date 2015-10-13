module Folder.List.Component where

import Prelude
import Halogen

import Control.Plus (Plus)
import Control.Apply ((*>))
import Data.Const (Const(..))
import Data.Functor.Coproduct (Coproduct(), coproduct)
import Data.Generic (Generic, gEq, gCompare)
import Data.Void (Void())

import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

import Folder.List.Item.Model (Item(..), initialItem)
import Folder.List.Item.Query (ItemQuery(..))
import Folder.List.Model (List(..), createItem, removeItem)
import Folder.List.Query (ListQuery(..))
import Folder.List.Item.Component (itemComponent)

data ItemSlot = ItemSlot Int

derive instance genericItemSlot :: Generic ItemSlot
instance eqItemSlot :: Eq ItemSlot where eq = gEq
instance ordItemSlot :: Ord ItemSlot where compare = gCompare

listParent :: forall g. (Functor g) => ParentComponentP List Item ListQuery ItemQuery g ItemSlot Void
listParent = component' render eval peek
  where

  render :: Render List ListQuery ItemSlot
  render list =
    H.div_ [ H.h2_ [ H.text list.title ]
           , H.p_ [ H.button [ E.onClick (E.input_ CreateItem) ]
                             [ H.text "Create item" ]
                  ]
           , H.ul_ (map (H.slot <<< ItemSlot) list.items)
           ]

  eval :: EvalP ListQuery List Item ListQuery ItemQuery g ItemSlot Void
  eval (CreateItem next) = modify createItem *> pure next
  eval (RemoveList next) = pure next

  peek :: Peek (ChildF ItemSlot ItemQuery) List Item ListQuery ItemQuery g ItemSlot Void
  peek (ChildF (ItemSlot itemId) (RemoveItem _)) = modify $ removeItem itemId
  peek _ = pure unit

type ListP g = InstalledState List Item ListQuery ItemQuery g ItemSlot Void
type ListQueryP = Coproduct ListQuery (ChildF ItemSlot ItemQuery)

listComponent :: forall g. (Plus g) => Component (ListP g) ListQueryP g Void
listComponent = install' listParent createItemChild
  where
  createItemChild :: ItemSlot -> ChildState Item ItemQuery g Void
  createItemChild (ItemSlot itemId) = createChild itemComponent (initialItem itemId)
