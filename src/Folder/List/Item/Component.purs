module Folder.List.Item.Component where

import Prelude

import Halogen
import Data.Void (Void())
import Control.Apply ((*>))

import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Properties.Indexed as P

import Folder.List.Item.Model (Item(..))
import Folder.List.Item.Query (ItemQuery(..))

itemComponent :: forall g. (Functor g) => Component Item ItemQuery g Void
itemComponent = component render eval
  where

  render :: Render Item ItemQuery Void
  render item =
    H.li_ [ H.input [ P.inputType P.InputCheckbox
                    , P.checked item.checked
                    , E.onChecked (E.input SetChecked)
                    ]
          , H.input [ P.inputType P.InputText
                    , P.value item.text
                    , E.onValueChange (E.input SetText)
                    ]
          , H.button [ E.onClick (E.input_ RemoveItem) ]
                     [ H.text "Remove item" ]
          ]

  eval :: Eval ItemQuery Item ItemQuery g
  eval (SetChecked checked next) = modify (_ { checked = checked }) *> pure next
  eval (SetText text next) = modify (_ { text = text }) *> pure next
  eval (RemoveItem next) = pure next
  eval (GetItem continue) = do
    item <- get
    pure $ continue item
