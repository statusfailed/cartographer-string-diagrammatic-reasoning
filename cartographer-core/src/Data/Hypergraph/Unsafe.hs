-- | Unsafe operations on hypergraphs.
module Data.Hypergraph.Unsafe
  ( addEdge
  , deleteEdge
  , connect
  ) where

import Data.Hypergraph.Type as Hypergraph
import Data.List (foldl')

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
  :: (Eq (f (sig, HyperEdgeId)), Ord (f (sig, HyperEdgeId)))
  => Port sig Source f
  -- ^ source port
  -> Port sig Target f
  -- ^ target port
  -> Hypergraph f sig
  -- ^ Hypergraph to modify
  -> Hypergraph f sig
-- overwrites connection if p1 or p2 was already connected!
connect p1 p2 hg = hg { connections = Bimap.insert p1 p2 (connections hg) }

-- | Delete a connection by its 'Source' port.
disconnectSource
  :: (Eq (f (sig, HyperEdgeId)), Ord (f (sig, HyperEdgeId)))
  => Port sig Source f
  -> Hypergraph f sig
  -> Hypergraph f sig
disconnectSource s hg = hg { connections = Bimap.delete s (connections hg) }

-- | delete a connection by its 'Target' port.
disconnectTarget
  :: (Eq (f (sig, HyperEdgeId)), Ord (f (sig, HyperEdgeId)))
  => Port sig Target f
  -> Hypergraph f sig
  -> Hypergraph f sig
disconnectTarget t hg = hg { connections = Bimap.deleteR t (connections hg) }
