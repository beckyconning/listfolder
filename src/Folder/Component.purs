module Folder.Component where

import Prelude
import Halogen

import Control.Plus (Plus)
import Control.Apply ((*>))
import Data.Const (Const(..))
import Data.Maybe (Maybe(..))
import Data.Void (Void())
import Data.Functor.Coproduct (Coproduct(), coproduct)
import Data.Generic (Generic, gEq, gCompare)

import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Events.Handler as EH
import qualified Halogen.HTML.Properties.Indexed as P

import Folder.Model (Folder(..), createList, removeList)
import Folder.Query (FolderQuery(..))
import Folder.List.Model (List(..), initialList)
import Folder.List.Query (ListQuery(..))
import Folder.List.Component (listComponent, ListP(), ListQueryP())

data ListSlot = ListSlot String Int

derive instance genericListSlot :: Generic ListSlot
instance eqListSlot :: Eq ListSlot where eq = gEq
instance ordListSlot :: Ord ListSlot where compare = gCompare


folderParent :: forall g. (Functor g) => ParentComponentP Folder (ListP g) FolderQuery ListQueryP g ListSlot Void
folderParent = component' render eval peek
  where

  render :: Render Folder FolderQuery ListSlot
  render folder =
    H.div_ [ H.h1_ [ H.text "Your lists" ]
           , H.form [ E.onSubmit (\_ -> EH.preventDefault *> pure (action CreateList)) ]
                    [ H.input [ E.onValueInput (E.input SetNextListTitle)
                              , P.value folder.nextListTitle
                              ]
                    , H.button_ [ H.text "Create list" ]
                    ]
           , H.ul_ (map renderList folder.lists)
           ]

    where

    renderList :: { listId :: Int, title :: String } -> HTML ListSlot FolderQuery
    renderList list =
        H.li [ P.class_ (if folder.currentListId == Just list.listId then visClass else hiddenClass) ]
             [ H.slot $ ListSlot list.title list.listId ]

    visClass = H.className "visible"
    hiddenClass = H.className "hidden"

  eval :: EvalP FolderQuery Folder (ListP g) FolderQuery ListQueryP g ListSlot Void
  eval (CreateList next) = modify createList *> pure next
  eval (ChooseList choice next) = modify (_ { currentListId = choice }) *> pure next
  eval (SetNextListTitle title next) = modify (_ { nextListTitle = title }) *> pure next

  peek :: Peek (ChildF ListSlot ListQueryP) Folder (ListP g) FolderQuery ListQueryP g ListSlot Void
  peek (ChildF (ListSlot _ listId) q) = coproduct peekList (const (pure unit)) q
    where
    peekList :: Peek ListQuery Folder (ListP g) FolderQuery ListQueryP g ListSlot Void
    peekList (RemoveList _) = modify $ removeList listId
    peekList _ = pure unit
  peek _ = pure unit

type FolderP g = InstalledState Folder (ListP g) FolderQuery ListQueryP g ListSlot Void
type FolderQueryP = Coproduct FolderQuery (ChildF ListSlot ListQueryP)

folderComponent :: forall g. (Plus g) => Component (FolderP g) FolderQueryP g Void
folderComponent = install' folderParent createListChild
  where

  createListChild :: ListSlot -> ChildState (ListP g) ListQueryP g Void
  createListChild (ListSlot title listId) =
    createChild listComponent (installedState (initialList listId title))
