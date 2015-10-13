module Main where

import Prelude
import Halogen

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Control.Monad.Aff (runAff)
import Halogen.Util (appendToBody)

import Folder.Component (folderComponent)
import Folder.Model (initialFolder)

main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) do
  app <- runUI folderComponent (installedState initialFolder)
  appendToBody app.node
