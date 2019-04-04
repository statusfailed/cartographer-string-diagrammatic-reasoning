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

import Cartographer.Viewer (ViewerOptions(..), Generator(..), RawAction(..))
import qualified Cartographer.Viewer as Viewer

import qualified Cartographer.Editor as Editor
import qualified Cartographer.Components.GeneratorEditor as GeneratorEditor

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

bialgebra :: [Generator]
bialgebra = [py, copy, unit, counit]

runOps xs = foldl (flip (.)) id xs Layout.empty

operations =
  [ snd . Layout.placeGenerator copy (V2 0 0)
  , snd . Layout.placeGenerator unit (V2 0 1)
  ]

testLayout = runOps operations

-------------------------------
-- Miso code

data Model = Model
  { editor    :: Editor.Model
  , generator :: Generator
  } deriving(Eq, Ord, Show)

{-emptyModel = Model (Editor.emptyModel { Editor._modelLayout = testLayout })-}
emptyModel = Model Editor.emptyModel copy

-- | Sum type for application events
data Action
  = NoOp
  | EditorAction Editor.Action
  | GeneratorEditorAction GeneratorEditor.Action
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
updateModel action m@(Model editor g) = case action of
  NoOp -> return m
  EditorAction a ->
    traceShow a . traceShow m . return $ m { editor = Editor.update a editor }
  GeneratorEditorAction ga -> return $   m { generator = GeneratorEditor.update ga g }

viewModel :: Model -> View Action
viewModel (Model editor g) = Miso.div_ []
  [ Miso.h1_ [] ["cartographer"]
  , GeneratorEditorAction <$> GeneratorEditor.view g
  , Miso.h1_ [] ["editor"]
  , EditorAction <$> Editor.view bialgebra editor
  ]
