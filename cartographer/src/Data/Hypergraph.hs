{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Hypergraph where

import Data.Graph

import Data.Set (Set(..))

import Data.Sequence (Seq(..), (!?))
import qualified Data.Sequence as Seq

import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Array (Array)
import qualified Data.Array as Array

import Data.Equivalence (Equivalence)
import qualified Data.Equivalence as Equivalence

import Data.Foldable

class Signature a where
  toSize :: a -> (Int, Int)

instance Integral a => Signature (a, a) where
  toSize (x, y) = (fromIntegral x, fromIntegral y)

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
data Hypergraph sig = Hypergraph
  { underlying :: Graph
  -- ^ The underlying connectivity graph
  , hyperedges :: Equivalence Vertex HyperEdgeId
  -- ^ Which hyperedge a vertex belongs to.
  -- A 'Vertex' not in this map is a node.
  , signatures :: Map HyperEdgeId sig
  , boundary   :: Boundary
  -- ^ Which Vertices are on the left and right boundaries
  , nextVertexId :: Vertex
  -- ^ Next "free" vertex Id.
  } deriving(Eq, Ord, Read, Show)

-- | The identity morphism is a hypergraph with a single node, appearing in
-- both the left and right boundaries.
identity :: Hypergraph sig
identity = Hypergraph
  { underlying    = Array.listArray (0,0) [[]]
  , signatures    = Map.empty
  -- graph with one node and no edges
  , hyperedges    = Equivalence.empty
  , boundary      = (Seq.singleton 0, Seq.singleton 0)
  , nextVertexId  = 1
  }

-- | The empty hypergraph is the hypergraph with no nodes or edges.
-- Unfortunately, Data.Array cannot represent an empty array, so this is
-- actually unrepresentable as a 'Hypergraph'!
--empty :: Hypergraph sig
--empty = error "TODO: underlying empty graph" $ Hypergraph
--  { underlying    = undefined
--  , signatures    = Map.empty
--  , hyperedges    = Equivalence.empty
--  , boundary      = (Seq.empty, Seq.empty)
--  , nextVertexId  = 0
--  }

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
isHyperEdge :: Vertex -> Hypergraph sig -> Bool
isHyperEdge v g = Equivalence.member v (hyperedges g)

-- | All the vertices in the graph which correspond to Nodes (as opposed to
-- hyper-edges)
nodes :: Hypergraph sig -> [Vertex]
nodes g = filter (not . flip isHyperEdge g)  . vertices . underlying $ g

-- | Add an edge, but don't connect it to anything.
-- TODO: do nothing if ID is already in graph!
--
-- This creates a n * m fully-connected bipartite graph, which is inserted into
-- the underlying graph, but not connected up to it (i.e. creating a new n+m
-- node disconnected subgraph)
{-# WARNING addEdge "will cause invalid behaviour if duplicate ID supplied" #-}
addEdge
  :: Signature sig
  => HyperEdgeId
  -> sig
  -> Hypergraph sig
  -> Hypergraph sig
addEdge e sig g =
  Hypergraph
  { underlying   = extend (underlying g) vs
  , hyperedges   = newHyperedges
  , signatures   = Map.insert e sig (signatures g)
  , boundary     = boundary g
  , nextVertexId = end + 1 -- TODO: check me
  }
  where
    (i, j) = toSize sig
    -- We create i+j new vertices, corresponding to each of the ports of the
    -- new hyperedge.
    start  = nextVertexId g -- First new ID
    middle = start + i - 1  -- Largest "left boundary" ID
    end    = middle + j     -- Largest "right boundary" ID

    inputs  = [ start .. middle ]
    outputs = [ middle + 1 .. end ]
    vs      = replicate i outputs ++ replicate j []

    newHyperedges =
      foldr (\v -> Equivalence.equate v e) (hyperedges g) (inputs ++ outputs)

-- | Try to get the 'Vertex' corresponding to a particular 'Port', specified to
-- be either the source (Left) or target (Right).
--
-- TODO: tidy this up!
portToVertex
  :: Signature sig => Either Port Port -> Hypergraph sig -> Maybe Vertex
portToVertex port graph = do
  case port of
    Right (Boundary i)  -> fst (boundary graph) !? i
    Right (Port e i)    ->
      Set.lookupMin $ Equivalence.membersOf e (hyperedges graph)

    Left (Boundary i)   -> snd (boundary graph) !? i
    Left (Port e i)     -> do
      dims    <- toSize <$> Map.lookup e (signatures graph)
      minPort <- Set.lookupMin $ Equivalence.membersOf e (hyperedges graph)
      return (minPort + fst dims)


-- | connect two 'HyperEdge's together by a specified port.  This amounts to
-- inserting a node into the hypergraph which is connected to a port of each
-- hyperedge
--
-- NOTE: this function connects two hyperedges together by a specified port.
-- It will verify that:
--   1) If the ports are already connected (to each other or anything else)
--      the connection is removed (2 edges + a vertex)
--   2) the former port is not reachable from the latter
connect
  :: Signature sig
  => Port
  -- ^ Source port: either LHS
  -> Port
  -- ^ Target port
  -> Hypergraph sig
  -> Maybe (Hypergraph sig)
connect source target graph = do
  v1 <- portToVertex (Left source) graph
  v2 <- portToVertex (Right target) graph
  return $ connect' v1 v2 graph

-- PROBLEM:
--   If we have the identity HG, with boundary ([0], [0])
--   and that 0 -> 0
--   When we connect up a new edge, say (1, 1), to the first boundary,
--   we have to replace the RHS [0] with [fresh], or otherwise change
--   the size of the boundary!
--   The real underlying problem here- we can't actually CONSTRUCT the
--   identity or twist graphs using these operations! no way to identify nodes
--   on the left boundary with ones on the right!
--
--   Suppose we had blank graph- add two boundary nodes (v0, v1), now connect-
--   This is NOT the same as the identity. Is it even legal in the paper?
--
-- | Unsafely connect together two vertices of the underlying Graph.
-- This creates a *directed* edge from v1 to v2, and ERASES any other edge from
-- v1.
-- Additionally:
--    - if v1 is a Boundary node, then remove it from RIGHT boundary
--    - if v2 is a Boundary node, then remove it from LEFT  boundary
-- NOTE: the above is done before connecting up the vertices.
--
-- NOTE: this function doesn't check much, so it's possible to really break the
-- hypergraph structure here- for example, if you connect' a LHS port of a
-- generator to something, it will completely screw up the graph structure.
--
-- NOTE ALSO: this function is /partial/. If v1 or v2 are not in the graph,
-- the program will crash.
connect' :: Vertex -> Vertex -> Hypergraph sig -> Hypergraph sig
connect' v1 v2 g' = g { underlying = updated }
  where
    g       = removeLeftBoundary v2 . removeRightBoundary v1 $ g'
    ug      = underlying g
    updated = ug Array.// [(v1, [v2])]

-- | Remove a Vertex from the left boundary, if present.
-- Otherwise, do nothing.
removeLeftBoundary :: Vertex -> Hypergraph sig -> Hypergraph sig
removeLeftBoundary v g = g { boundary = onFst (Seq.filter (==v)) (boundary g) }
  where
    onFst f (x, y) = (f x, y)

-- | Remove a Vertex from the right boundary, if present.
removeRightBoundary :: Vertex -> Hypergraph sig -> Hypergraph sig
removeRightBoundary v g = g { boundary = onSnd (Seq.filter (==v)) (boundary g) }
  where
    onSnd f (x, y) = (x, f y)

addBoundaryNode :: Hypergraph sig -> (Vertex, Hypergraph sig)
addBoundaryNode g = undefined

-- try to remove a boundary node. If the Vertex is not on the boundary, or is
-- not a node, throw an error (or do nothing?)
removeBoundaryNode :: Vertex -> Hypergraph sig -> Hypergraph sig
removeBoundaryNode v g = undefined

-------------------------------
-- Predicates

-- | Check if the hyperedges in a hypergraph are all connected to nodes.
-- That is, every vertex corresponding to a hyperedge port is connected to a
-- vertex.
connected :: Hypergraph sig -> Bool
connected g = undefined
