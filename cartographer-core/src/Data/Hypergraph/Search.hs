{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Data.Hypergraph.Search where

import Data.Reflection
import Data.Maybe
import Data.Hypergraph.Type

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- | Given a set of starting nodes, do an undirected depth-first search to find
-- all reachable nodes.
-- TODO: tidy this up, it\'s a bit nasty!
undirectedDfs :: Ord a => OpenHypergraph a -> [Wire a Open] -> [Wire a Open]
undirectedDfs g = go Set.empty
  where
    go _ [] = []
    go visited (cur:stack) = case Set.member cur visited of
      True  -> go visited stack
      False -> cur : go (Set.insert cur visited) (adjacent g cur ++ stack)

-- | Nodes that are one step away from the given node
-- NOTE: we *dont* want to consider boundary ports to be adjacent to each
-- other!
adjacent :: Ord a => OpenHypergraph a -> Wire a Open -> [Wire a Open]
adjacent g (Port s _, Port t _) =
  uncurry (++) (f g s) ++ uncurry (++) (f g t)
  where
    f g s = open ([], [])  (wires g . Gen) s

-- | Input and output wires of a given hyperedge.
wires
  :: Ord (f (sig, HyperEdgeId))
  => Hypergraph f sig
  -> f (sig, HyperEdgeId)
  -> ([Wire sig f], [Wire sig f])
wires g fe = (inputs, outputs)
  where
    p i = Port fe i
    inputs  =
      f [ (,p i) <$> Bimap.lookupR (p i) (connections g) | i <- [0..] ]
    outputs  =
      f [ (p i,) <$> Bimap.lookup (p i) (connections g) | i <- [0..] ]
    f = catMaybes . takeWhile isJust
