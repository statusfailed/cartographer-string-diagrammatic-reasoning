{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Miso
import Miso.String (MisoString(..))
import Data.String

import Cartographer.Layout (Layout)
import qualified Cartographer.Layout as Layout

import Data.Hypergraph (Hypergraph, Open(..))
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
data Model = Model
  { layout :: Layout Generator
  , numOps :: Int
  } deriving(Eq, Ord, Read, Show)

operations :: [Layout Generator -> Layout Generator]
operations =
  [ snd . Layout.placeGenerator counit  1 1 4 -- 0
  , snd . Layout.placeGenerator py      3 1 0 -- 1
  , snd . Layout.placeGenerator copy    3 0 2 -- 2
  , snd . Layout.placeGenerator unit    1 0 1 -- 3
  , snd . Layout.placeGenerator counit  1 1 8 -- 4
  , Layout.connectPorts (Hypergraph.Port (Gen 2) 0) (Hypergraph.Port (Gen 1) 0)
  , Layout.connectPorts (Hypergraph.Port (Gen 3) 0) (Hypergraph.Port (Gen 0) 0)
  ]

runOperations xs = foldl (flip (.)) id xs Layout.empty

-- TODO: right now we use the layout with a single 1x1 generator, placed at the
-- origin, which is 1 grid-square tall.
example0 :: Layout Generator
example0 = runOperations (take 4 operations)

example1 :: Layout Generator
example1 = runOperations operations

emptyModel :: Model
emptyModel = Model (runOperations operations) (length operations)

-- | Sum type for application events
data Action = NoOp | SetNumOps Int
  deriving (Eq, Ord, Read, Show)

-- | Entry point for a miso application
main :: IO ()
main = do
  startApp App {..}
  where
    initialAction = NoOp
    model  = emptyModel
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs   = []
    mountPoint = Nothing

updateModel :: Action -> Model -> Effect Action Model
updateModel action m = case action of
  NoOp -> noEff m
  SetNumOps n ->
    noEff $ m { numOps = n, layout = runOperations (take n operations) }

viewModel :: Model -> View Action
viewModel (Model layout numOps) = div_ []
  [ button_ [ onClick (SetNumOps $ dec numOps) ] [ "<<" ]
  , button_ [ onClick (SetNumOps $ inc numOps) ] [ ">>" ]
  , div_ []  [ viewLayout layout (ViewOptions 50) ]
  , div_ [] [ fromString (show $ Layout.connectors layout) ]
  ]
  where
    inc n = min (n + 1) (length operations)
    dec n = max 0 (n - 1)
