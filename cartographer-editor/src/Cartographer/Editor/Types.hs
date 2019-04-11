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
  , _modelHighlights  :: MatchState Generator
  } deriving(Eq, Ord, Show)

fromLayout :: Layout Generator -> Model
fromLayout l = Model l Done emptyMatchState

emptyModel :: Model
emptyModel = Model Layout.empty Done emptyMatchState

-- | The set of actions in the Editor.
-- Note that the action of "connecting ports" is decided based on the
-- 'Viewer.Action'.
data Action
  = ViewerAction Viewer.Action
  | ClearDiagram -- ^ clear the diagram
  | StartPlaceGenerator Generator
  | StartDeleteGenerator  -- ^ start deleting a generator
  | StartMoveGenerator    -- ^ start moving a generator
  | StartDisconnect
  deriving(Eq, Ord, Read, Show)

-- | 'ActionState' keeps track of what the user is trying to achieve for
-- multi-state actions, such as connecting two ports.
data ActionState
  = Done
  -- ^ Nothing to do
  | Connecting     (V2 Int) (These (Port Source Open) (Port Target Open))
  -- ^ Clicked a source and/or target port at a given position
  | PlaceGenerator Generator
  -- ^ Clicked a generator button
  | DeleteGenerator
  | MoveGenerator
  | MoveGeneratorFrom (Layout.Tile HyperEdgeId)
  | Disconnect
  deriving(Eq, Ord, Read, Show)
