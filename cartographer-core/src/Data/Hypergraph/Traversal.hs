{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
module Data.Hypergraph.Traversal
  ( outputWires
  , inputWires
  , wireBfs
  , children
  , parents
  , bfsAcyclic
  , wireBfsAcyclic
  , bfs
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

-- | Breadth first traversal of wires
wireBfs
  :: Signature sig
  => OpenHypergraph sig -> Set (Wire Open) -> [Wire Open] -> [[Wire Open]]
wireBfs _ _ [] = []
wireBfs hg visited current = current : wireBfs hg visited' current'
  where
    visited' = Set.union visited (Set.fromList current)
    current' = filter (not . flip Set.member visited') (current >>= children hg)

-- | Get the immediate descendant wires of a 'Wire'- i.e., all the wires coming
-- out of the generator which the 'Wire's 'Target' 'Port' is connected to.
children :: Signature sig => OpenHypergraph sig -> Wire Open -> [Wire Open]
children hg (_, Port Boundary _) = []
children hg (_, Port (Gen e) _ ) = outputWires hg e

parents :: Signature sig => OpenHypergraph sig -> Wire Open -> [Wire Open]
parents hg (Port Boundary _, _) = []
parents hg (Port (Gen e) _, _) = inputWires hg e

bfsAcyclic :: Signature sig => OpenHypergraph sig -> [[Wire Open]]
bfsAcyclic hg = take maxPathLen $ wireBfsAcyclic hg (start hg)
  where
    start = filter (isInitial hg . fst) . Bimap.toList . connections
    maxPathLen = Bimap.size . connections $ hg

wireBfsAcyclic
  :: Signature sig
  => OpenHypergraph sig -> [Wire Open] -> [[Wire Open]]
wireBfsAcyclic _ [] = []
wireBfsAcyclic hg current =
  current : wireBfsAcyclic hg (current >>= children hg)

-- | Does the hyperedge of a port have zero inputs
isInitial :: Signature sig => OpenHypergraph sig -> Port a Open -> Bool
isInitial hg (Port Boundary _) = True
isInitial hg (Port (Gen e)  _) =
  maybe False hasNoInputs . Map.lookup e . signatures $ hg
  where
    hasNoInputs = (==0) . fst . toSize


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
    -- start from the left boundary.
    -- TODO: this is a really dumb way to get the left boundary.
    -- for a n → m graph, just do n lookups for each Port Boundary [0..n]
    start = filter (isBoundary . fst) . Bimap.toList . connections
