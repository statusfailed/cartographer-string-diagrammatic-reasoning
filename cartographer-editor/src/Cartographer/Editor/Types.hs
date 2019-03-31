module Cartographer.Editor.Types where

import Miso.String (MisoString)
import Data.Hypergraph

import Cartographer.Layout (Layout(..))
import qualified Cartographer.Layout as Layout

import Cartographer.Viewer (Generator(..))
import qualified Cartographer.Viewer as Viewer

data Model = Model
  { _modelLayout      :: Layout Generator
  , _modelActionState :: ActionState
  } deriving(Eq, Ord, Show)

data Action
  = ViewerAction Viewer.Action
  | StartConnect
  | StartPlaceGenerator Generator
  deriving(Eq, Ord, Read, Show)

-- | 'ActionState' keeps track of what the user is trying to achieve for
-- multi-state actions, such as connecting two ports.
--
-- Here's an example trace:
--  user clicks Connect tool:  (ConnectSource, NoResult)
--  user clicks a source port, s: (ConnectTarget s, NoResult)
--  user clicks a target port, t: (NoAction, connectPorts s t layout)
data ActionState
  = NoAction
  | ConnectSource
  | ConnectTarget (Port Source Open)
  | PlaceGenerator Generator
  deriving(Eq, Ord, Read, Show)

update :: Model -> Action -> Model
update (Model layout actionState) action = case action of
  ViewerAction va ->
    let (as, f) = updateActionState actionState va
    in  Model (f layout) as

  StartConnect          -> Model layout ConnectSource
  StartPlaceGenerator g -> Model layout (PlaceGenerator g)

updateActionState
  :: ActionState
  -> Viewer.Action
  -> (ActionState, Layout Generator -> Layout Generator)
updateActionState as a = undefined
{-case (as, a) of-}
  {-(ConnectSource, Action _ (ClickedPorts _ _ (Just o))) ->-}
    {-(ConnectTarget i, id)-}
  {-(ConnectTarget i, Action _ (ClickedPorts _ (Just i) _)) ->-}
    {-(ConnectTarget o, Layout.connectPorts i o)-}
  {-(PlaceGenerator -}
