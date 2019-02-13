module Data.Hypergraph where

import Data.Graph

import Data.Set (Set(..))
import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map

import Data.Foldable

data HyperEdgeId = HyperEdgeId
  deriving(Eq, Ord, Read, Show)

-- | The 'Hypergraph' type ... ?
--
-- NB: This type does not insist that every edge is connected to a node;
-- this is so that users can edit the hypergraph incrementally. That is,
-- new generators can be added without immediately connecting them.
--
-- We model hypergraphs as ordinary graphs, where each node is either a
-- hypergraph node, or a "port" of a hyperedge.
data Hypergraph = Hypergraph
  { underlying :: Graph
  -- ^ The underlying connectivity graph
  , hyperedges :: Map Vertex HyperEdgeId
  -- ^ Which hyperedge a vertex belongs to.
  -- A 'Vertex' not in this map is a node.
  } deriving(Eq, Ord, Read, Show)

-- v `isEdgeOf` g returns True iff v is a hyperedge in the hypergraph g.
isHyperEdge :: Hypergraph -> Vertex -> Bool
isHyperEdge g v = Map.member v (hyperedges g)

nodes :: Hypergraph -> [Vertex]
nodes g = filter (not . isHyperEdge g)  . vertices . underlying $ g

-- | Add an edge, but don't connect.
addEdge :: HyperEdgeId -> Hypergraph -> Hypergraph
addEdge e g = undefined

-- | connect two edges together by a specified port.  This amounts to inserting
-- a node into the hypergraph which is connected to a port of each hyperedge
--
-- NOTE: this function will verify that:
--   1) 
connect :: Vertex -> Vertex -> Hypergraph -> Hypergraph
connect = undefined

connect' :: (HyperEdgeId, Port) -> (HyperEdgeId, Port) -> Hypergraph -> Hypergraph

-------------------------------
-- Predicates

-- | Check if the hyperedges in a hypergraph are all connected to nodes.
connected :: Hypergraph -> Bool
connected g = undefined
