{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
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
  ( Hypergraph, Port(..), PortRole(..), Open(..), Source, Target
  , OpenHypergraph(..), HyperEdgeId(..)
  )
import qualified Data.Hypergraph as Hypergraph

import Cartographer.Types.Grid (Grid, Position)
import qualified Cartographer.Types.Grid as Grid

import Data.Equivalence (Equivalence)
import qualified Data.Equivalence as Equivalence

import Data.Reflection

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
  } deriving(Eq, Ord, Show)

-- | The empty layout state. An empty hypergraph, nothing positioned, and no
-- hyperedges yet created.
empty :: Layout sig
empty = Layout
  { hypergraph      = Hypergraph.empty
  , grid            = Grid.empty
  , nextHyperEdgeId = 0
  }

-- | Width/Height of the Layout in tiles
-- TODO: account for number of boundary nodes
dimensions :: Layout sig -> V2 Int
dimensions = V2 2 0 + Grid.dimensions . grid

-- | Insert a generator into a specific layer, at a particular offset.
-- If it would overlap with another generator, the generators are shifted down.
-- TODO: rename placeGenerator to something more consistent?
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
--
-- TODO:
-- If any of the following are true, the connection is not made:
--    * ports are invalid
--    * L(target) <= L(source)
--
-- Additionally, A maximum if L(target) - L(source) - 1 pseudonodes are
-- inserted into the grid.
{-# WARNING connectPorts "partial function" #-}
connectPorts
  :: Port Source Open
  -- ^ Source port
  -> Port Target Open
  -- ^ Target port
  -> Layout sig
  -> Layout sig
connectPorts s t layout = layout'
  { hypergraph = Hypergraph.connect s t (hypergraph layout') }
  where
    cleanS l =
      if Hypergraph.target s (hypergraph l) /= Just t
        then disconnectSource s l
        else l
    cleanT l =
      if Hypergraph.source t (hypergraph l) /= Just s
        then disconnectTarget t l
        else l

    base l = maybe err id (generatorPosition s l)
      where err = error $ "connectPorts: lookup error " ++ show s

    addPseudo pn@(PseudoNode _ _ i) l = addPseudoNode pn (base l + offset) l
      where offset = V2 i 0

    withPseudos l = foldl (flip addPseudo) l $ connectionPseudoNodes s t l

    layout' = withPseudos . cleanS . cleanT $ layout

-------------------------------
-- TODO: NOTE: there is definitely a nicer way to write the below two
-- functions.....
-- Can we make "disconnect" operate on a WIRE instead? (i.e. pair of ports?)
-------------------------------

-- | Remove all the pseudonodes for a given source port
-- if the port has no target, do nothing.
disconnectSource
  :: Port Source Open -> Layout sig -> Layout sig
disconnectSource s l = case Hypergraph.target s (hypergraph l) of
  Just t  -> foldl (flip removePseudoNode) l (connectionPseudoNodes s t l)
  Nothing -> l

-- | Remove all the pseudonodes for a given target port
-- if the port has no source, do nothing.
disconnectTarget
  :: Port Target Open -> Layout sig -> Layout sig
disconnectTarget t l = case Hypergraph.source t (hypergraph l) of
  Just s  -> foldl (flip removePseudoNode) l (connectionPseudoNodes s t l)
  Nothing -> l

-------------------------------
-- Pseudnodes

-- | Add a pseudonodes into the grid.
addPseudoNode :: PseudoNode -> Position -> Layout sig -> Layout sig
addPseudoNode pseudo v layout = layout
  { grid = Grid.placeTile (TilePseudoNode pseudo) 1 v (grid layout) }

-- | Remove a pseudonode from the grid
removePseudoNode :: PseudoNode -> Layout sig -> Layout sig
removePseudoNode pseudo layout = layout
  { grid = Grid.removeTile (TilePseudoNode pseudo) (grid layout) }

-- | Compute which pseudonodes must exist for a given connection
connectionPseudoNodes
  :: Port Source Open -> Port Target Open -> Layout sig -> [PseudoNode]
connectionPseudoNodes source target layout = maybe [] id $ do
  n <- layersBetween source target layout
  return $ fmap (PseudoNode source target) [0..n - 1]

-- | Number of layers separating two ports.
-- Returns Nothing if ports
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

--------------------------------------------------------------
-- Utilities

-- | A thin wrapper around Grid.positions
-- TODO: replace this interface to return a Map (Tile (Port () Open)) Position ?
-- Then automatically insert Boundary "generators" and shift all non-boundaries
-- right by 1.
-- Computing the Map (Tile HyperEdgeId) Position would just be a simple
-- filter + fmap on this map...
positions :: Layout sig -> Map (Tile HyperEdgeId) Position
positions = Grid.positions . grid

-- | Get the position of a port's generator.
generatorPosition
  :: Reifies a PortRole
  => Port a Open -> Layout sig -> Maybe Position
generatorPosition p l = case p of
  Port Boundary i -> return (V2 bx i) -- bx depends on port role
  Port (Gen e) i  -> fmap (+ V2 1 i) . Grid.positionOf (TileHyperEdge e) $ g
  where
    g       = grid l
    V2 w h  = Grid.dimensions g
    bx      = if Hypergraph.toPortRole p == Source then 0 else w + 2

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
