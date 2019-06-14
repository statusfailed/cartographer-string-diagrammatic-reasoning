-- | Unsafe operations on hypergraphs.
module Data.Hypergraph.Unsafe
  ( addEdge
  , deleteEdge
  , connect
  , incrementHyperEdgeIds
  , mergeR
  ) where

import Data.Hypergraph.Type as Hypergraph

import Control.Arrow ((***))

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

-------------------------------
-- Unsafely manipulate and build hypergraphs.

-- | Add an edge to a 'Hypergraph'.
-- TODO: don't let user of this module assign the hyperedge ID-
-- they could easily break the graph (and replace an existing generator, which
-- might end up with fewer ports, and then weird invalid connections)
addEdge :: sig -> Hypergraph f sig -> (HyperEdgeId, Hypergraph f sig)
addEdge sig g = (edgeId, g
  { connections     = connections g -- new edge is unconnected
  , signatures      = Map.insert edgeId sig (signatures g)
  , nextHyperEdgeId = succ edgeId 
  })
  where
    edgeId = nextHyperEdgeId g

-- | Delete an edge from a 'Hypergraph'
deleteEdge
  :: (Applicative f, Eq (f HyperEdgeId), Ord (f HyperEdgeId))
  => HyperEdgeId -> Hypergraph f sig -> Hypergraph f sig
deleteEdge e g = g
  { connections = removeWiresOf e (connections g)
  , signatures  = Map.filterWithKey (\k _ -> k /= e) (signatures g)
  }
  where
    removeWiresOf e =
      Bimap.filter (\s t -> not $ s `isPortOf` e || t `isPortOf` e)

-- | Connect two ports in the hypergraph.
-- If the source or target port was already connected to something, that
-- connection is overwritten.
--
-- As-is, this does not prevent cycles.
-- CARTOGRAPHER relies on the Layout class to enforce this, by only allowing
-- ports in shallower layers to connect to deeper ones.
connect
  :: (Eq (f HyperEdgeId), Ord (f HyperEdgeId))
  => Port Source f
  -- ^ source port
  -> Port Target f
  -- ^ target port
  -> Hypergraph f sig
  -- ^ Hypergraph to modify
  -> Hypergraph f sig
-- overwrites connection if p1 or p2 was already connected!
connect p1 p2 hg = hg { connections = Bimap.insert p1 p2 (connections hg) }

-- | Delete a connection by its 'Source' port.
disconnectSource
  :: (Eq (f HyperEdgeId), Ord (f HyperEdgeId))
  => Port Source f
  -> Hypergraph f sig
  -> Hypergraph f sig
disconnectSource s hg = hg { connections = Bimap.delete s (connections hg) }

-- | delete a connection by its 'Target' port.
disconnectTarget
  :: (Eq (f HyperEdgeId), Ord (f HyperEdgeId))
  => Port Target f
  -> Hypergraph f sig
  -> Hypergraph f sig
disconnectTarget t hg = hg { connections = Bimap.deleteR t (connections hg) }

-------------------------------
-- Very unsafe functions

-- | Add a fixed, positive integer amount to all HyperEdgeIds.
--
-- NOTE: this only adds so we can use mapKeysMonotonic.  Since we're rebuilding
-- the Bimap anyway, we might as well generalize to any function?
incrementHyperEdgeIds
  :: HyperEdgeId -> OpenHypergraph sig -> OpenHypergraph sig
incrementHyperEdgeIds k hg@(Hypergraph c s n)
  | k <= 0 = hg
  | otherwise = Hypergraph c' s' n'
  where
    c' = Bimap.fromList
          (fmap (mapPortEdge (+k) *** mapPortEdge (+k)) . Bimap.toList $ c)
    s' = Map.mapKeysMonotonic (+k) s
    n' = k + n

mapPortEdge :: Functor f => (HyperEdgeId -> HyperEdgeId) -> Port a f -> Port a f
mapPortEdge f (Port x i) = Port (fmap f x) i

-- | Merge one 'OpenHypergraph' into another.
-- 'mergeR a b' will add all the edges and connections of b into a, without
-- checking for overwriting, etc.
--
-- You probably don't want to use this unless you know for sure the edge IDs
-- are disjoint.
mergeR :: OpenHypergraph sig -> OpenHypergraph sig -> OpenHypergraph sig
mergeR a b = undefined
