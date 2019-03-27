{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
-- | "Abstract" drawing of a 'Layout'.
-- this module gives enough functionality to make drawing a layout trivial.
-- Using it, you can get:
--
--    * A list of 'Tile's, and their positions (in pixels).
--    * A list of connectors, and *their* positions.
module Cartographer.Draw where

import Data.Hypergraph
  (Hypergraph, HyperEdgeId, Port(..), PortRole(..), Open(..), Source, Target)
import qualified Data.Hypergraph as Hypergraph

import Cartographer.Layout (Layout, Tile(..))
import qualified Cartographer.Layout as Layout

import Cartographer.Types.Grid (Grid, Position)
import qualified Cartographer.Types.Grid as Grid

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

import Data.Reflection

import Linear.Vector ((*^))
import Linear.V2 (V2(..))

-- [(Tile, Position)]
-- [(Position,Position)]

-- | All information required to render the Layout
-- TODO: missing sig information (what sig information?)
-- TODO: missing Boundary positions!
-- TODO: missing tile dimensions!
-- TODO: missing matchings!
data Renderable sig v = Renderable
  { tiles :: [(Tile sig, v)]
  -- ^ Both "real" tiles and Pseudonodes (but what about boundaries!?)
  , wires :: [(v, v)]
  -- ^ The set of wires in the form (source, target).
  -- This list does *not* contain connections that span multiple layers.
  , dimensions :: V2 Int
  -- ^ width/height of the grid (in tiles)
  } deriving(Eq, Ord, Read, Show, Functor)

-- TODO: FIXME (finish implementation)
toGridCoordinates :: Layout sig -> Renderable sig Position
toGridCoordinates l = Renderable
  { tiles = toTiles l
  , wires = toWires (\_ v i -> v + V2 0 i) l
  , dimensions = Layout.dimensions l
  }

-- TODO: gross, rewrite D:
{-# WARNING toTiles "partial function" #-}
toTiles :: Layout sig -> [(Tile sig, Position)]
toTiles layout = fmap f . Map.toList . Layout.positions $ layout
  where
    m = Hypergraph.signatures . Layout.hypergraph $ layout
    f (t, v) = case t of
      TileHyperEdge  e -> case Map.lookup e m of
        Just r  -> (TileHyperEdge r, v + V2 1 0)
        Nothing -> error $ "toTiles: missing key " ++ show e
      TilePseudoNode p -> (TilePseudoNode p, v + V2 1 0)

-- NOTE: This function returns a list of wires between ADJACENT tiles.
--       It's a solution to the problem of long wires crossing generators
--
-- It works like this:
--  1) break each "long wire" into a number of "intermediate" wires
--      * Intermediate wire has the form Tile (Port a Open) ?
--  2) for each intermediate wire, look up its endpoints, e.g.:
--      * TileHyperEdge (Port Boundary i)  ---> TilePseudoNode pn
--      * (V2 0 i) ---> Grid.position pn
toWires
  :: (sig -> V2 Int -> Int -> V2 Int)
  -- ^ Find the location of a port, given the generator location, port role,
  -- and port index.
  -> Layout sig
  -> [(Position, Position)]
toWires portPosition layout =
  flip wirePosition layout <$> (breakWire' =<< allWires layout)
  where
    breakWire' = flip breakWire layout

-------------------------------
-- toWires: helper functions

type Wire = (Port Source Open, Port Target Open)

-- Get the "raw" connectivity information from the layout - i.e. all wires,
-- including those that span multiple layers.
allWires :: Layout sig -> [Wire]
allWires = Bimap.toList . Hypergraph.connections . Layout.hypergraph

-------------------------------
-- Breaking up wires

-- An 'IntermediateWire' is a wire between either a "real" tile, or a
-- pseudonode.
type IntermediateWire = (Endpoint Source, Endpoint Target)

-- An endpoint is either a pseudonode port (in which case it's 1x1), or
-- a hyperedge port.
type Endpoint a = Tile (Port a Open)

-- Break a long wire into intermediates. An intermediate goes between two
-- endpoints, where an endpoint is either a port or a pseudonode.
breakWire :: Wire -> Layout sig -> [IntermediateWire]
breakWire (source, target) layout = zip sources targets where
  -- list of pseudonodes between source and target ports.
  pseudos   = Layout.connectionPseudoNodes source target layout
  -- source and target endpoint lists
  sources = TileHyperEdge source : (TilePseudoNode <$> pseudos)
  targets = (TilePseudoNode <$> pseudos) ++ [TileHyperEdge target]

{-# WARNING wirePosition "partial function" #-}
wirePosition
  :: IntermediateWire
  -> Layout sig
  -- ^ TODO: FIXME: cache information like this map and diagram width and use
  -- reader monad to tidy up!
  -> (Position, Position)
wirePosition (s, t) layout = (ps, pt) where
  ps = endpointPosition s layout
  pt = endpointPosition t layout

{-# WARNING endpointPosition "partial function" #-}
-- | Get the tile position of an 'Endpoint'
endpointPosition
  :: Reifies a PortRole
  => Endpoint a -> Layout sig -> Position
endpointPosition e layout = case e of
  TileHyperEdge p   -> case Layout.portPosition (\_ _ i -> V2 0 0) p layout of
    Just  v -> v
    Nothing -> error $ "endpointPosition: missing hyperedge posn or sig"
  TilePseudoNode pn -> case Layout.positionOf (TilePseudoNode pn) layout of
    Just r -> r + V2 1 0
    Nothing -> error $ "endpointPosition: missing key " ++ show pn
