module Cartographer.Editor.Model where

import Miso.String (MisoString)
import Data.Hypergraph
import Linear.V2 (V2(..))

import Cartographer.Layout (Layout(..))
import qualified Cartographer.Layout as Layout

import Cartographer.Viewer (Generator(..), ClickedPorts(..))
import qualified Cartographer.Viewer as Viewer

import Cartographer.Editor.Types as Editor

-- | Update the Editor model without side-effects
update :: Editor.Action -> Editor.Model -> Editor.Model
update action (Model layout actionState) = case action of
  ViewerAction va ->
    let (as, f) = updateActionState actionState va
    in  Model (f layout) as

  StartConnect          -> Model layout ConnectSource
  StartPlaceGenerator g -> Model layout (PlaceGenerator g)

-- | A state machine for handling inputs
--
-- TODO: this would be a billion times more readable with Lens.
updateActionState
  :: ActionState
  -> Viewer.Action
  -> (ActionState, Layout Generator -> Layout Generator)
updateActionState ConnectSource s = case s of
  Viewer.Action _ (Just (ClickedPorts _ _ (Just o))) ->
    (ConnectTarget o, id)
  _ -> (Done, id)

updateActionState s@(ConnectTarget o) a = case a of
  Viewer.Action _ (Just (ClickedPorts _ (Just i) _)) ->
    (Done, Layout.connectPorts o i)
  _ -> (Done, id)

updateActionState (PlaceGenerator g) a = case a of
  Viewer.Action pos _ ->
    (Done, snd . Layout.placeGenerator g (spacedToExtended pos))
  _ -> (Done, id)

updateActionState Done a = (Done, id)

-- | Turned spaced coordinates into extended coordinates (see DESIGN.md)
--
-- TODO: this should probably go in Viewer, or ViewerAction should do it for
-- us.
spacedToExtended :: V2 Int -> V2 Int
spacedToExtended (V2 x y) = V2 (x `div` 2) y
