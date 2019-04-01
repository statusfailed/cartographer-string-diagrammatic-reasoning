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
    (Done, insertSpacedCoordinates g pos)

updateActionState Done a = (Done, id)

-- | Turned spaced coordinates into extended coordinates (see DESIGN.md)
--
-- TODO: this should probably go in Viewer, or ViewerAction should do it for
-- us.
spacedToExtended :: V2 Int -> V2 Int
spacedToExtended (V2 x y) = V2 (x `div` 2) y

-- | Insert into the Layout in spaced coordinates.
-- When inserting onto a "wires" column, a layer is first inserted into the
-- extended grid to the *right* of the wires column.
-- Otherwise, the generator is inserted into column clicked.
insertSpacedCoordinates
  :: Layout.Generator sig
  => sig -> V2 Int -> Layout sig -> Layout sig
insertSpacedCoordinates g v@(V2 0 _) = id -- TODO: still ignore left boundary?
insertSpacedCoordinates g v@(V2 x _) = snd . Layout.placeGenerator g v' . f
  where
    f = if odd x then Layout.insertLayer (Layout.Layer x') else id
    v'@(V2 x' _) = spacedToExtended (v - V2 1 0)
