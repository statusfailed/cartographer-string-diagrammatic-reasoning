{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Miso
import Miso.String

import Cartographer.Layout (Layout)
import qualified Cartographer.Layout as Layout

import Data.Hypergraph (Hypergraph)
import qualified Data.Hypergraph as Hypergraph

import View
import Types

-------------------------------
-- Test generators

altId :: Generator
altId = Generator (1,1) ([0], [0]) "black" "altId"

py :: Generator
py = Generator (2, 1) ([0, 2], [1]) "white" "py"

copy :: Generator
copy = Generator (1, 2) ([1], [0,2]) "black" "copy"

unit :: Generator
unit = Generator (0, 1) ([], [0]) "black" "unit"

counit :: Generator
counit = Generator (1, 0) ([0], []) "black" "counit"

-------------------------------
-- Miso code

-- | Type synonym for an application model
type Model = Layout Generator

-- TODO: right now we use the layout with a single 1x1 generator, placed at the
-- origin, which is 1 grid-square tall.
example0 :: Layout Generator
example0
  = id
  . snd . Layout.placeGenerator unit    1 0 0
  . snd . Layout.placeGenerator copy    3 0 2
  . snd . Layout.placeGenerator py      3 1 0
  . snd . Layout.placeGenerator counit  1 1 4
  $ Layout.empty

example1 :: Layout Generator
example1
  = id
  . Layout.connectPorts (Hypergraph.Port 3 0) (Hypergraph.Port 1 0)
  . snd . Layout.placeGenerator unit    1 0 0
  . snd . Layout.placeGenerator copy    3 0 2
  . snd . Layout.placeGenerator py      3 1 0
  . snd . Layout.placeGenerator counit  1 1 4
  $ Layout.empty

emptyModel :: Layout Generator
emptyModel = example1

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
