{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | The main interface for editing a Hypergraph via a UI.
-- the 'Layout' type includes position and pseudonode information for each
-- hyperedgein the graph, and provides methods for safely making changes to the underlying graph.
--
-- /This module is intended to be imported qualified./
module Cartographer.Layout where

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

import Linear.V2

import Data.Hypergraph (Hypergraph, HyperEdgeId(..))
import qualified Data.Hypergraph as Hypergraph

import Cartographer.Types.Grid (Grid, Position)
import qualified Cartographer.Types.Grid as Grid

import Cartographer.Types.Equivalence (Equivalence)
import qualified Cartographer.Types.Equivalence as Equivalence

newtype Layer = Layer { unLayer :: Int }
  deriving(Eq, Ord, Read, Show, Enum, Num)

newtype Offset = Offset { unOffset :: Int }
  deriving(Eq, Ord, Read, Show, Enum, Num)

fromLayerOffset :: Layer -> Offset -> V2 Int
fromLayerOffset (Layer i) (Offset j) = V2 i j

-- | a 'Hypergraph' plus the additional information needed to display it laid
-- out in 2D.
-- NOTE that we
data Layout sig = Layout
  { hypergraph :: Hypergraph
  -- ^ the underlying hypergraph to be laid out
  , signatures :: Map HyperEdgeId sig
  -- ^ "shape" of each hyperedge in the graph (defines number of ports)
  , positions  :: Grid HyperEdgeId
  -- ^ Position of each HyperEdge in the layout.

  -- , portMap    :: Map (HyperEdgeId, Port) Vertex
  -- mapping between ports and vertices in the underlying graph
  -- TODO: put this in the Hypergraph type, or make them sequential, i.e.
  -- a generator of type (n, m) will have n+m vertices, numbered 0..n+m?

  -- next free HyperEdgeId
  , nextHyperEdgeId :: HyperEdgeId
  -- ^ Next free ID to add a HyperEdge.
  } deriving(Eq, Ord, Read, Show)

-- TODO: can't easily have empty layout because of underlying graph problem
-- (i.e. no empty arrays allowed :|)
empty :: Layout sig
empty = Layout
  { hypergraph      = Hypergraph.identity -- TODO?
  , signatures      = Map.empty
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
  where lookupSigs (edgeId, pos) = (signatures layout ! edgeId, pos)

-- | Insert a generator into a specific layer, at a particular offset.
-- If it would overlap with another generator, the generators are shifted down.
placeGenerator
  :: sig
  -- ^ What kind of generator?
  -> (Int, Int)
  -- ^ ports on left + right boundary.
  -> Grid.Height
  -- ^ How many grid-squares tall is it?
  -> Layer
  -- ^ What layer to put it in?
  -> Offset
  -- ^ At what offset?
  -> Layout sig
  -> (HyperEdgeId, Layout sig)
placeGenerator sig dims height layer offset l = (nextId, l') where
  edgeId = nextHyperEdgeId l
  nextId = succ edgeId
  l' = Layout
    { hypergraph = Hypergraph.addEdge edgeId dims (hypergraph l)
    -- Add new edgeId to hypergraph

    , signatures = Map.insert edgeId sig (signatures l)
    -- Add signature to signatures

    , positions =
        Grid.placeTile edgeId height
          (fromLayerOffset layer offset) (positions l)
    -- Finally, placeTile in Grid to update positions.

    , nextHyperEdgeId = nextId
    -- Assign new HyperEdgeId and return it
    }

connectPorts
  :: Hypergraph.Port
  -- ^ Source. Either a LEFT Boundary node, or a RHS port of a hyperedge
  -> Hypergraph.Port
  -- ^ Target. Either a RIGHT Boundary node, or a LHS port of a hyperedge
  -> Layout sig
  -> Layout sig
connectPorts s t l = undefined


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
