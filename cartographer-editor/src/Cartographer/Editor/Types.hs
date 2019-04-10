module Cartographer.Editor.Types where

import Miso.String (MisoString)
import Data.Hypergraph
import Linear.V2 (V2(..))

import Cartographer.Layout (Layout(..))
import qualified Cartographer.Layout as Layout

import Cartographer.Viewer (Generator(..), ClickedPorts(..))
import qualified Cartographer.Viewer as Viewer

import Data.These

data Model = Model
  { _modelLayout      :: Layout Generator
  , _modelActionState :: ActionState
  } deriving(Eq, Ord, Show)

fromLayout :: Layout Generator -> Model
fromLayout l = Model l Done

emptyModel :: Model
emptyModel = Model Layout.empty Done

-- | The set of actions in the Editor.
-- Note that the action of "connecting ports" is decided based on the
-- 'Viewer.Action'.
data Action
  = ViewerAction Viewer.Action
  | StartPlaceGenerator Generator
  deriving(Eq, Ord, Read, Show)

-- | 'ActionState' keeps track of what the user is trying to achieve for
-- multi-state actions, such as connecting two ports.
--
-- TODO: remove this
-- Here's an example trace:
--  user clicks Connect tool:  (ConnectSource, NoResult)
--  user clicks a source port, s: (ConnectTarget s, NoResult)
--  user clicks a target port, t: (NoAction, connectPorts s t layout)
data ActionState
  = Done
  -- ^ Nothing to do
  | Connecting     (These (Port Source Open) (Port Target Open))
  -- ^ Clicked a source and/or target port.
  | PlaceGenerator Generator
  -- ^ Clicked a generator button
  deriving(Eq, Ord, Read, Show)
