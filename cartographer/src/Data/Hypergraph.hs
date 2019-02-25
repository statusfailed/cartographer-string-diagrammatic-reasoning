{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Hypergraph where

import Data.Graph

import Data.Set (Set(..))

import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq

import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map

import Data.Array (Array)
import qualified Data.Array as Array

import Data.Foldable

-- | Uniquely identify each edge of a hypergraph.
-- Does not say what the "shape" of each generator is.
newtype HyperEdgeId = HyperEdgeId { unHyperEdgeId :: Int }
  deriving(Eq, Ord, Read, Show, Enum, Num)

type Boundary = (Seq Vertex, Seq Vertex)

-- A port references either a port of a hyperedge, or a boundary node.
-- Whether it is left or right boundary depends on context.
data Port = Port HyperEdgeId Int | Boundary Int
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
  -- TODO: shouldn't this be an Equivalence?
  , boundary   :: Boundary
  -- ^ Which Vertices are on the left and right boundaries
  , nextVertexId :: Vertex
  -- ^ Next "free" vertex Id.
  } deriving(Eq, Ord, Read, Show)

-- | The identity morphism is a hypergraph with a single node, appearing in
-- both the left and right boundaries.
identity :: Hypergraph
identity = Hypergraph
  { underlying    = Array.listArray (0,0) [[]]
  -- graph with one node and no edges
  , hyperedges    = Map.empty
  , boundary      = (Seq.singleton 0, Seq.singleton 0)
  , nextVertexId  = 1
  }

-- | The empty hypergraph is the hypergraph with no nodes or edges.
-- Unfortunately, Data.Array cannot represent an empty array, so this is
-- actually unrepresentable as a 'Hypergraph'!
empty :: Hypergraph
empty = error "TODO: underlying empty graph" $ Hypergraph
  { underlying    = undefined
  , hyperedges    = Map.empty
  , boundary      = (Seq.empty, Seq.empty)
  , nextVertexId  = 0
  }

-- | Extend an Array with additional elements
--
-- NOTE: this is rubbish and slow for updating because it copies the whole
-- array. But I have to use it if I want to use Data.Graph! :)
extend :: (Array.Ix i, Integral i) => Array i v -> [v] -> Array i v
extend a xs = Array.array (lb, ub+k) newElems
  where
    (lb, ub) = Array.bounds a
    k        = fromIntegral $ length xs
    newElems = (Array.assocs a ++ zipWith (\i x -> (ub+i, x)) [1..] xs)

-- isHyperEdge g v returns True iff v is a hyperedge in the hypergraph g.
isHyperEdge :: Vertex -> Hypergraph -> Bool
isHyperEdge v g = Map.member v (hyperedges g)

-- | All the vertices in the graph which correspond to Nodes (as opposed to
-- hyper-edges)
nodes :: Hypergraph -> [Vertex]
nodes g = filter (not . flip isHyperEdge g)  . vertices . underlying $ g

-- | Add an edge, but don't connect it to anything.
-- TODO: do nothing if ID is already in graph!
--
-- This creates a n * m fully-connected bipartite graph, which is inserted into
-- the underlying graph, but not connected up to it (i.e. creating a new n+m
-- node disconnected subgraph)
{-# WARNING addEdge "will cause invalid behaviour if duplicate ID supplied" #-}
addEdge :: HyperEdgeId -> (Int, Int) -> Hypergraph -> Hypergraph
addEdge e (i, j) g =
  Hypergraph
  { underlying   = extend (underlying g) vs
  , hyperedges   = foldr (\v -> Map.insert v e) (hyperedges g) (inputs++outputs)
  , boundary     = boundary g
  , nextVertexId = nextVertexId g + i + j - 1
  }
  where
    -- We create i+j new vertices, corresponding to each of the ports of the
    -- new hyperedge.
    inputs  = [ nextVertexId g     .. nextVertexId g + i - 1]
    outputs = [ nextVertexId g + i .. nextVertexId g + i + j - 1 ]
    vs      = replicate i outputs ++ replicate j []

-- | connect two 'HyperEdge's together by a specified port.  This amounts to
-- inserting a node into the hypergraph which is connected to a port of each
-- hyperedge
--
-- NOTE: this function connects two hyperedges together by a specified port.
-- It will verify that:
--   1) If the ports are already connected (to each other or anything else)
--      the connection is removed (2 edges + a vertex)
--   2) the former port is not reachable from the latter
connect :: Vertex -> Vertex -> Hypergraph -> Hypergraph
connect = undefined

-- | A More useful interface for 'connect'.
connect'
  :: (HyperEdgeId, Port)
  -- ^ Source port
  -> (HyperEdgeId, Port)
  -- ^ Target port
  -> Hypergraph
  -> Hypergraph
connect' = undefined

addBoundaryNode :: Hypergraph -> (Vertex, Hypergraph)
addBoundaryNode g = undefined

-- try to remove a boundary node. If the Vertex is not on the boundary, or is
-- not a node, throw an error (or do nothing?)
removeBoundaryNode :: Vertex -> Hypergraph -> Hypergraph
removeBoundaryNode v g = undefined

-------------------------------
-- Predicates

-- | Check if the hyperedges in a hypergraph are all connected to nodes.
-- That is, every vertex corresponding to a hyperedge port is connected to a
-- vertex.
connected :: Hypergraph -> Bool
connected g = undefined
