{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Miso
import Miso.String

import Cartographer.Layout (Layout)
import qualified Cartographer.Layout as Layout

import View

-- | Type synonym for an application model
type Model = Layout ()

-- TODO: right now we use the layout with a single 1x1 generator, placed at the
-- origin, which is 1 grid-square tall.
emptyModel :: Layout ()
emptyModel = snd $ Layout.placeGenerator () (1,1) 1 0 0 Layout.empty

-- | Sum type for application events
data Action = Action
  deriving (Eq, Ord, Read, Show)

-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
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
