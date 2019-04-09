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
  ( Hypergraph, HyperEdgeId, Port(..), PortRole(..)
  , Wire(..), Open(..), Source, Target)
import qualified Data.Hypergraph as Hypergraph

import Cartographer.Layout (Layout, Tile(..), PseudoNode(..))
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
  , wires :: [(Wire Open, (v, v))]
  -- The list of wire segments to draw.
  -- this list contains the wire being drawn, and the endpoints of the segment
  -- of that wire.
  , dimensions :: V2 Int
  -- ^ width/height of the grid (in tiles)
  } deriving(Eq, Ord, Read, Show, Functor)

-- TODO: FIXME (finish implementation)
toGridCoordinates
  :: Layout.Generator sig => Layout sig -> Renderable sig Position
toGridCoordinates l = Renderable
  { tiles = toTiles l
  , wires = toWires l
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
  :: Layout.Generator sig
  => Layout sig
  -> [(Wire Open, (Position, Position))]
toWires layout =
  flip wirePosition layout <$> (breakWire' =<< allWires layout)
  where
    breakWire' = flip breakWire layout

-------------------------------
-- toWires: helper functions

-- Get the "raw" connectivity information from the layout - i.e. all wires,
-- including those that span multiple layers.
allWires :: Layout sig -> [Wire Open]
allWires = Bimap.toList . Hypergraph.connections . Layout.hypergraph

-------------------------------
-- Breaking up wires

-- An 'IntermediateWire' is a wire between either a "real" tile, or a
-- pseudonode.
type IntermediateWire = (Endpoint Source, Endpoint Target)

-- An endpoint is either a pseudonode port (in which case it's 1x1), or
-- a hyperedge port.
type Endpoint a = Tile (Port a Open)

-- | get the port of an 'Endpoint'
endpointToSource :: Endpoint Source -> Port Source Open
endpointToSource (TileHyperEdge p) = p
endpointToSource (TilePseudoNode (PseudoNode s _ _)) = s

endpointToTarget :: Endpoint Target -> Port Target Open
endpointToTarget (TileHyperEdge p) = p
endpointToTarget (TilePseudoNode (PseudoNode _ t _)) = t

-- Break a long wire into intermediates. An intermediate goes between two
-- endpoints, where an endpoint is either a port or a pseudonode.
breakWire :: Wire Open -> Layout sig -> [IntermediateWire]
breakWire (source, target) layout = zip sources targets where
  -- list of pseudonodes between source and target ports.
  pseudos   = Layout.connectionPseudoNodes source target layout
  -- source and target endpoint lists
  sources = TileHyperEdge source : (TilePseudoNode <$> pseudos)
  targets = (TilePseudoNode <$> pseudos) ++ [TileHyperEdge target]

{-# WARNING wirePosition "partial function" #-}
wirePosition
  :: Layout.Generator sig
  => IntermediateWire
  -> Layout sig
  -- ^ TODO: FIXME: cache information like this map and diagram width and use
  -- reader monad to tidy up!
  -> (Wire Open, (Position, Position))
wirePosition (s, t) layout = (w, (ps, pt)) where
  ps = endpointPosition s layout
  pt = endpointPosition t layout
  w  = (endpointToSource s, endpointToTarget t)

{-# WARNING endpointPosition "partial function" #-}
-- | Get the tile position of an 'Endpoint'
endpointPosition
  :: (Layout.Generator sig, Reifies a PortRole)
  => Endpoint a
  -> Layout sig
  -> Position
endpointPosition e layout = case e of
  TileHyperEdge p   -> case Layout.portPosition p layout of
    Just  v -> v
    Nothing -> error $ "endpointPosition: missing hyperedge posn or sig: " ++ show p
  TilePseudoNode pn -> case Layout.positionOf (TilePseudoNode pn) layout of
    Just r -> r + V2 1 0
    Nothing -> error $ "endpointPosition: missing key " ++ show pn
