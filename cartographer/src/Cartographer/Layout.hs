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
import Control.Monad (liftM2)

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

-- | A pseudonode is uniquely identified by the two ports it helps connect,
-- and its offset from the source node.
-- Its x-position is exactly the source port's x position plus the offset.
data PseudoNode = PseudoNode (Port Source Open) (Port Target Open) Int
  deriving(Eq, Ord, Read, Show)

-- | Tiles are user-manipulable 1xN shapes laid out on the grid.  A tile is
-- either a 1xN generator (Hyperedge) or a 1x1 Pseudonode (identity)
data Tile a
  = TileHyperEdge a
  | TilePseudoNode PseudoNode
  deriving(Eq, Ord, Read, Show)

fromLayerOffset :: Layer -> Offset -> V2 Int
fromLayerOffset (Layer i) (Offset j) = V2 i j

-- | a 'Hypergraph' plus the additional information needed to display it laid
-- out in 2D.
-- NOTE that we
data Layout sig = Layout
  { hypergraph :: OpenHypergraph sig
  -- ^ the underlying hypergraph to be laid out
  , grid       :: Grid (Tile HyperEdgeId) -- previously Grid HyperEdgeId
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
  , grid            = Grid.empty
  , nextHyperEdgeId = 0
  }

-- | Width/Height of the Layout in tiles
-- TODO: don't use a fixed "buffer" of 5 for all tiles' heights!
dimensions :: Layout sig -> V2 Int
dimensions = Grid.dimensions . grid

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

    , grid =
        Grid.placeTile (TileHyperEdge edgeId) height
          (fromLayerOffset layer offset) (grid l)
    -- Finally, placeTile in Grid to update positions.

    , nextHyperEdgeId = nextId
    -- Assign new HyperEdgeId and return it
    }

-- | connect two hypergraph ports in the layout.
-- NOTE: returns the original graph unchanged if ports were invalid.
-- TODO
-- If two ports are not adjacent (i.e., target is not immediately to the right
-- of source), then pseudonodes are also inserted into the Grid.
connectPorts
  :: Port Source Open
  -- ^ Source port
  -> Port Target Open
  -- ^ Target port
  -> Layout sig
  -> Layout sig
connectPorts s t layout
  = layout { hypergraph = Hypergraph.connect s t (hypergraph layout) }

-- | Number of layers separating two ports.
layersBetween :: Port Source Open -> Port Target Open -> Layout sig -> Maybe Int
layersBetween s t l
  = liftM2 (\t s -> t - s - 1) (target t) (source s)
  where
    target = g (width + 1)
    source = g (-1)

    g bpos (Port Boundary _) = Just bpos
    g _ (Port (Gen e) _) =
      fmap getX . Map.lookup (TileHyperEdge e) . Grid.positions . grid $ l

    width = getX (dimensions l)
    getX (V2 x _) = x


-- | Compute which pseudonodes must exist for a given connection
connectionPseudoNodes
  :: Port Source Open -> Port Target Open -> Layout sig -> [PseudoNode]
connectionPseudoNodes source target layout = maybe [] id $ do
  n <- layersBetween source target layout
  return $ fmap (PseudoNode source target) [0..n]

--------------------------------------------------------------
-- Drawing

-- | A thin wrapper around Grid.positions
-- TODO: replace this interface to return a Map (Tile (Port () Open)) Position ?
-- Then automatically insert Boundary "generators" and shift all non-boundaries
-- right by 1.
-- Computing the Map (Tile HyperEdgeId) Position would just be a simple
-- filter + fmap on this map...
positions :: Layout sig -> Map (Tile HyperEdgeId) Position
positions = Grid.positions . grid

-- TODO: consider if this is the right interface.
-- NOTE: Leaving unsafe use of ! in the datastructure, because if it ever
-- fails then there are bugs elsewhere!
positioned :: Ord sig => Layout sig -> [(sig, Position)]
positioned layout =
  fmap lookupSigs . sigTiles . Map.toList . Grid.positions . grid $ layout
  where
    lookupSigs (edgeId, pos) = (Hypergraph.signatures hg ! edgeId, pos)
    hg = hypergraph layout

    sigTiles = catMaybes . fmap f
      where
        f (TileHyperEdge e, pos) = Just (e, pos)
        f (TilePseudoNode _, pos) = Nothing


-------------------------------
-- Wire layout helpers
-- TODO: most of this needs to be rethought

-- | Position of a port in the layout.
--
positionOf
  :: Position -- ^ Position of boundary's 0th tile.
  -> Port a Open -- ^ Port to find position of
  -> Layout sig
  -> Maybe Position
positionOf boundary p l = case p of
  -- TODO! correct port location :)
  (Port (Gen e) i) ->
    fmap (+ V2 0 i) (Grid.positions (grid l) !? (TileHyperEdge e))
  (Port Boundary i) -> Just (boundary + V2 0 i)

-- | Return the positions of two ports, if they're adjacent.
-- NOTE: this is badly named- it isn't quite "adjacent" - this will return
-- Nothing if L(target) <= L(source)
adjacent
  :: Port Source Open
  -> Port Target Open
  -> Layout sig
  -> Maybe (Position, Position)
adjacent source target layout = do
  v1 <- positionOf (V2 0 0)     source layout
  v2 <- positionOf (V2 width 0) target layout
  case v2 - v1 of
    V2 1 _  -> Just (v1, v2)
    _       -> Nothing
  where (V2 width _) = dimensions layout + V2 1 0

connectors :: Layout sig -> [(Position, Position)]
connectors layout = catMaybes . fmap (($layout) . uncurry adjacent) $ xs
  where xs = Map.toList . Hypergraph.connections . hypergraph $ layout

-------------------------------
-- TODO

-- | Insert a new Layer, corresponding to a new column.
insertLayer :: Layer -> Layout sig -> Layout sig
insertLayer i l = undefined
  -- Shift everything >= i up one layer
  -- Recompute pseudonodes for all connections:

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
