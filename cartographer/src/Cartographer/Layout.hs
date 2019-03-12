{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | The main interface for editing a Hypergraph via a UI.
-- the 'Layout' type includes position and pseudonode information for each
-- hyperedgein the graph, and provides methods for safely making changes to the underlying graph.
--
-- /This module is intended to be imported qualified./
module Cartographer.Layout where

import Data.Map.Strict (Map, (!), (!?))
import qualified Data.Map.Strict as Map

import Linear.V2
import Data.Maybe (catMaybes)

import Data.Hypergraph
  ( Hypergraph, Port(..), Open(..), Source, Target
  , OpenHypergraph(..), HyperEdgeId(..)
  )
import qualified Data.Hypergraph as Hypergraph

import Cartographer.Types.Grid (Grid, Position)
import qualified Cartographer.Types.Grid as Grid

import Data.Equivalence (Equivalence)
import qualified Data.Equivalence as Equivalence

newtype Layer = Layer { unLayer :: Int }
  deriving(Eq, Ord, Read, Show, Enum, Num)

newtype Offset = Offset { unOffset :: Int }
  deriving(Eq, Ord, Read, Show, Enum, Num)

-- | Tiles are user-manipulable 1xN shapes laid out on the grid.
-- They either
data Tile
  = Generator HyperEdgeId
  | Pseudo (Port Source Open) (Port Target Open)
  deriving(Eq, Ord, Read, Show)

fromLayerOffset :: Layer -> Offset -> V2 Int
fromLayerOffset (Layer i) (Offset j) = V2 i j

-- | a 'Hypergraph' plus the additional information needed to display it laid
-- out in 2D.
-- NOTE that we
data Layout sig = Layout
  { hypergraph :: OpenHypergraph sig
  -- ^ the underlying hypergraph to be laid out
  , positions  :: Grid HyperEdgeId
  -- ^ Position of each HyperEdge in the layout.
  , nextHyperEdgeId :: HyperEdgeId
  -- ^ Next free ID to add a HyperEdge.
  -- TODO: put this in Hypergraph?
  } deriving(Eq, Ord, Read, Show)

-- | The empty layout state. An empty hypergraph, nothing positioned, and no
-- hyperedges yet created.
empty :: Layout sig
empty = Layout
  { hypergraph      = Hypergraph.empty
  , positions       = Grid.empty
  , nextHyperEdgeId = 0
  }

-- | Width/Height of the Layout in tiles
-- TODO: don't use a fixed "buffer" of 5 for all tiles' heights!
dimensions :: Layout sig -> V2 Int
dimensions = Grid.dimensions . positions

-- TODO: consider if this is the right interface.
-- NOTE: Leaving unsafe use of ! in the datastructure, because if it ever
-- fails then there are bugs elsewhere!
positioned :: Ord sig => Layout sig -> [(sig, Position)]
positioned layout
  = fmap lookupSigs . Map.toList . Grid.positions . positions $ layout
  where
    lookupSigs (edgeId, pos) = (Hypergraph.signatures hg ! edgeId, pos)
    hg = hypergraph layout

-- | Insert a generator into a specific layer, at a particular offset.
-- If it would overlap with another generator, the generators are shifted down.
placeGenerator
  :: Hypergraph.Signature sig
  => sig
  -- ^ What kind of generator?
  -> Grid.Height
  -- ^ How many grid-squares tall is it? (TODO: work this out?)
  -> Layer
  -- ^ What layer to put it in?
  -> Offset
  -- ^ At what offset?
  -> Layout sig
  -> (HyperEdgeId, Layout sig)
placeGenerator sig height layer offset l = (nextId, l') where
  dims = Hypergraph.toSize sig
  edgeId = nextHyperEdgeId l
  nextId = succ edgeId
  l' = Layout
    { hypergraph = Hypergraph.addEdge edgeId sig (hypergraph l)
    -- Add new edgeId to hypergraph

    , positions =
        Grid.placeTile edgeId height
          (fromLayerOffset layer offset) (positions l)
    -- Finally, placeTile in Grid to update positions.

    , nextHyperEdgeId = nextId
    -- Assign new HyperEdgeId and return it
    }

-- | connect two hypergraph ports in the layout.
-- NOTE: returns the original graph unchanged if ports were invalid.
connectPorts
  :: Port Source Open
  -- ^ Source port
  -> Port Target Open
  -- ^ Target port
  -> Layout sig
  -> Layout sig
connectPorts s t layout
  = layout { hypergraph = Hypergraph.connect s t (hypergraph layout) }

-------------------------------
-- Wire layout helpers

-- | Position of a port in the layout.
-- TODO: finish this; it's not complete or correct
positionOf :: Port a Open -> Layout sig -> Maybe Position
positionOf p l = case p of
  -- TODO! correct port location :)
  (Port (Gen e) i) ->
    fmap (+ V2 0 i) (Grid.positions (positions l) !? e)
  _ -> undefined -- TODO: connecting boundaries!

-- | Return the positions of two ports, if they're adjacent.
-- NOTE: this is badly named- it isn't quite "adjacent" - this will return
-- Nothing if L(target) <= L(source)
adjacent
  :: Port Source Open
  -> Port Target Open
  -> Layout sig
  -> Maybe (Position, Position)
adjacent source target layout = do
  v1 <- positionOf source layout
  v2 <- positionOf target layout
  case v2 - v1 of
    V2 1 _  -> Just (v1, v2)
    _       -> Nothing

connectors :: Layout sig -> [(Position, Position)]
connectors layout = catMaybes . fmap (($layout) . uncurry adjacent) $ xs
  where xs = Map.toList . Hypergraph.connections . hypergraph $ layout

-------------------------------
-- TODO


-- | Insert a new Layer, corresponding to a new column.
insertLayer :: Layer -> Layout sig -> Layout sig
insertLayer i l = undefined
  -- Shift everything >= i up one layer
  -- Recompute pseudonodes


addBoundaryNode :: Either Int Int -> Layout sig -> Layout sig
addBoundaryNode = undefined
  -- Hypergraph.addBoundaryNode

-- Collision resolution for a generator of tileHeight n placed at position p:
--  * Check if p is occupied by another
--  *
--  1) Examine y-positions p, p+1, ... p+n
--  2) If any of those positions

-------------------------------
-- Post-MVP functionality

-- | Set a hyperedge's position.
--
-- Suppose an edge e has a parent p, and the user requests to move edge e to
-- layer x.
-- If L(e) >= x, then disconnect e from p.
move :: HyperEdgeId -> Position -> Layout sig -> Layout sig
move e p s = undefined

-- TODO post-mvp
removeBoundaryNode = undefined
