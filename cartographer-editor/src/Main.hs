{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Miso
import Miso.String

import Cartographer.Layout (Layout)
import qualified Cartographer.Layout as Layout

import View
import Types

-------------------------------
-- Test generators

altId :: Generator
altId = Generator (1,1) ([0], [0]) "altId"

py :: Generator
py = Generator (2, 1) ([0, 2], [1]) "py"

copy :: Generator
copy = Generator (1, 2) ([1], [0,2]) "copy"

unit :: Generator
unit = Generator (0, 1) ([], [0]) "unit"

counit :: Generator
counit = Generator (1, 0) ([0], []) "counit"

-------------------------------
-- Miso code

-- | Type synonym for an application model
type Model = Layout Generator

-- TODO: right now we use the layout with a single 1x1 generator, placed at the
-- origin, which is 1 grid-square tall.
emptyModel :: Layout Generator
emptyModel
  = id
  . snd . Layout.placeGenerator unit    (0,1) 1 0 0
  . snd . Layout.placeGenerator copy    (1,2) 3 0 2
  . snd . Layout.placeGenerator py      (2,1) 3 1 0
  . snd . Layout.placeGenerator counit  (1,0) 1 1 4
  $ Layout.empty

-- | Sum type for application events
data Action = Action
  deriving (Eq, Ord, Read, Show)

-- | Entry point for a miso application
main :: IO ()
main = do
  startApp App {..}
  where
    initialAction = Action
    model  = emptyModel
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs   = []
    mountPoint = Nothing

updateModel :: Action -> Model -> Effect Action Model
updateModel Action m = noEff m

viewModel :: Model -> View Action
viewModel m = viewLayout m (ViewOptions 50)
