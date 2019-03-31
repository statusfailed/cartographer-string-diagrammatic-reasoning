{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Miso
import Miso.String (MisoString(..), ms)
import Miso.Subscription.Keyboard (arrowsSub, Arrows(..))

import Cartographer.Layout (Layout)
import qualified Cartographer.Layout as Layout

import Data.Hypergraph (Hypergraph, Open(..))
import qualified Data.Hypergraph as Hypergraph

import Linear.V2 (V2(..))

import Cartographer.Viewer (ViewerOptions(..))
import qualified Cartographer.Viewer as Viewer
import Cartographer.Viewer.Types (Generator(..), RawAction(..))

import Debug.Trace (traceShow)

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
  } deriving(Eq, Ord, Show)

operations = pseudotest2

pseudotest :: [Layout Generator -> Layout Generator]
pseudotest =
  [ snd . Layout.placeGenerator unit   1 (V2 0 0)
  , snd . Layout.placeGenerator counit 1 (V2 2 1)
  , snd . Layout.placeGenerator copy   3 (V2 1 0) -- gets in way of pseudo!
  , Layout.connectPorts (Hypergraph.Port (Gen 0) 0) (Hypergraph.Port (Gen 1) 0)
  , Layout.connectPorts (Hypergraph.Port (Gen 0) 0) (Hypergraph.Port (Gen 2) 1)
  , Layout.connectPorts (Hypergraph.Port (Gen 2) 0) (Hypergraph.Port (Gen 1) 0)
  , Layout.connectPorts (Hypergraph.Port (Gen 0) 0) (Hypergraph.Port Boundary 2)
  , Layout.connectPorts (Hypergraph.Port Boundary 0) (Hypergraph.Port (Gen 2) 1)
  , Layout.connectPorts (Hypergraph.Port (Gen 2) 2) (Hypergraph.Port Boundary 0)
  ]

-- NOTE: the final command will crash the Layout.
-- This is because it causes missing pseudonodes- when the grid gets wider,
-- pseudos must be inserted.
pseudotest2 :: [Layout Generator -> Layout Generator]
pseudotest2 =
  [ snd . Layout.placeGenerator unit   1 (V2 0 0)
  , snd . Layout.placeGenerator copy   3 (V2 1 0) -- gets in way of pseudo!
  , Layout.connectPorts (Hypergraph.Port (Gen 0) 0) (Hypergraph.Port (Gen 1) 0)
  , Layout.connectPorts (Hypergraph.Port (Gen 0) 0) (Hypergraph.Port Boundary 2)
  , snd . Layout.placeGenerator counit 1 (V2 2 1)
  , Layout.connectPorts (Hypergraph.Port (Gen 1) 0) (Hypergraph.Port (Gen 2) 2)
  ]

bigtest :: [Layout Generator -> Layout Generator]
bigtest =
  [ snd . Layout.placeGenerator counit  1 (V2 1 4)
  , snd . Layout.placeGenerator py      3 (V2 1 0)
  , snd . Layout.placeGenerator copy    3 (V2 0 2)
  , snd . Layout.placeGenerator unit    1 (V2 0 0)
  , Layout.connectPorts (Hypergraph.Port (Gen 2) 0) (Hypergraph.Port (Gen 1) 0)
  , Layout.connectPorts (Hypergraph.Port (Gen 3) 0) (Hypergraph.Port (Gen 0) 0)
  -- Wires to boundaries
  -- TODO
  -- Wires requiring pseudonodes
  -- , snd . Layout.placeGenerator unit    1 2 0
  -- , Layout.connectPorts (Hypergraph.Port (Gen 2) 1) (Hypergraph.Port (Gen 4) 0)
  -- move vertical
  -- move horizontal
  --  move into "wire" column - creates new column.
  -- add column?
  -- move many?
  -- verify everything connected (before matching)
  ]

runOperations xs = foldl (flip (.)) id xs Layout.empty

-- TODO: right now we use the layout with a single 1x1 generator, placed at the
-- origin, which is 1 grid-square tall.
example0 :: Layout Generator
example0 = runOperations (take 4 operations)

example1 :: Layout Generator
example1 = runOperations operations

emptyModel :: Model
emptyModel = Model (runOperations []) 0 -- (runOperations operations) (length operations)

-- | Sum type for application events
data Action = NoOp | ViewerAction Viewer.Action | AddNumOps Int
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
    subs   = [arrowsSub arrowToAction]
    mountPoint = Nothing

arrowToAction :: Arrows -> Action
arrowToAction (Arrows x _) = AddNumOps x

updateModel :: Action -> Model -> Effect Action Model
updateModel action m = case action of
  NoOp -> noEff m
  AddNumOps k ->
    let n = clamp (numOps m + k)
    in
      noEff $ m { numOps = n
                , layout = runOperations (take n operations)
                }
  ViewerAction x -> traceShow x (return m)

  where
    clamp = max 0 . min (length operations)

viewModel :: Model -> View Action
viewModel m@(Model layout numOps) = div_ []
  [ button_ [ onClick (AddNumOps (-1)) ] [ "<<" ]
  , button_ [ onClick (AddNumOps 1   ) ] [ ">>" ]
  , div_ [] [ ViewerAction <$> Viewer.view layout (ViewerOptions 50) ]
  , div_ [] [ text (ms $ show m) ]
  ]
