{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
module Data.Hypergraph.Traversal
  ( bfsAcyclic
  , wireBfsAcyclic
  , bfs
  , wireBfs
  , bfsR
  , wireBfsR
  , children
  , parents
  , outputWires
  , inputWires
  , sourcePorts
  , targetPorts
  ) where

import Data.Hypergraph.Type
import Data.Maybe (catMaybes)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

bfsAcyclic :: Signature sig => OpenHypergraph sig -> [[Wire Open]]
bfsAcyclic hg = wireBfsAcyclic hg (start hg)
  where start = filter (isBoundary . fst) . Bimap.toList . connections

wireBfsAcyclic
  :: Signature sig
  => OpenHypergraph sig -> [Wire Open] -> [[Wire Open]]
wireBfsAcyclic _ [] = []
wireBfsAcyclic hg current =
  current : wireBfsAcyclic hg (current >>= children hg)

-- | Traverse an 'OpenHypergraph' breadth-first, beginning from its left
-- boundary.
-- TODO: document return value more :D
--
-- >>> bfs identity
-- [ [(Port Boundary 0, Port Boundary 0)] ]
--
bfs :: Signature sig => OpenHypergraph sig -> [[Wire Open]]
bfs hg = wireBfs hg Set.empty (start hg)
  where
    -- start from the left boundary
    start = filter (isBoundary . fst) . Bimap.toList . connections

bfsR :: Signature sig => OpenHypergraph sig -> [[Wire Open]]
bfsR hg = wireBfsR hg Set.empty (start hg)
  where
    start = filter (isBoundary . snd) . Bimap.toList . connections

-- | Breadth first traversal of wires
wireBfs
  :: Signature sig
  => OpenHypergraph sig -> Set (Wire Open) -> [Wire Open] -> [[Wire Open]]
wireBfs _ _ [] = []
wireBfs hg visited current = current : wireBfs hg visited' current'
  where
    visited' = Set.union visited (Set.fromList current)
    current' = filter (not . flip Set.member visited') (current >>= children hg)

-- | Breadth-first traversal of wires (in reverse)
wireBfsR
  :: Signature sig
  => OpenHypergraph sig -> Set (Wire Open) -> [Wire Open] -> [[Wire Open]]
wireBfsR _ _ [] = []
wireBfsR hg visited current = current : wireBfsR hg visited' current'
  where
    visited' = Set.union visited (Set.fromList current)
    current' = filter (not . flip Set.member visited') (current >>= parents hg)

-- | Get the immediate descendant wires of a 'Wire'- i.e., all the wires coming
-- out of the generator which the 'Wire's 'Target' 'Port' is connected to.
children :: Signature sig => OpenHypergraph sig -> Wire Open -> [Wire Open]
children hg (_, Port Boundary _) = []
children hg (_, Port (Gen e) _ ) = outputWires hg e

parents :: Signature sig => OpenHypergraph sig -> Wire Open -> [Wire Open]
parents hg (Port Boundary _, _) = []
parents hg (Port (Gen e) _, _) = inputWires hg e

-- | Given a HyperEdgeId, get all its outgoing wires.
-- NOTE: this can return the empty list if the hypergraph is not complete.
outputWires
  :: (Ord (f HyperEdgeId), Applicative f, Signature sig)
  => Hypergraph f sig -> HyperEdgeId -> [Wire f]
outputWires hg e = case Map.lookup e (signatures hg) of
  Just sig -> catMaybes . fmap (flip toWire hg) $ sourcePorts e sig
  Nothing  -> []

-- | Given a HyperEdgeId, get all its incoming wires.
inputWires
  :: (Ord (f HyperEdgeId), Applicative f, Signature sig)
  => Hypergraph f sig -> HyperEdgeId -> [Wire f]
inputWires hg e = case Map.lookup e (signatures hg) of
  Just sig -> catMaybes . fmap (flip toWire hg) $ targetPorts e sig
  Nothing  -> []

-- | A list of all source (output) ports of a hyperedge with a particular
-- signature.
sourcePorts
  :: (Applicative f, Signature sig)
  => HyperEdgeId -> sig -> [Port Source f]
sourcePorts e sig = fmap (Port (pure e)) [0..k - 1]
  where (_, k) = toSize sig

-- | A list of all target (inpu) ports of a hyperedge with a particular
-- signature.
targetPorts
  :: (Applicative f, Signature sig)
  => HyperEdgeId -> sig -> [Port Target f]
targetPorts e sig = fmap (Port (pure e)) [0..n - 1]
  where (n, _) = toSize sig
