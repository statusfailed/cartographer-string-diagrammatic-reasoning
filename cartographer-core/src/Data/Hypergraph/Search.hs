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

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- | Given a set of starting nodes, do an undirected depth-first search to find
-- all reachable nodes.
-- TODO: tidy this up, it\'s a bit nasty!
undirectedDfs :: OpenHypergraph a -> [Wire Open] -> [Wire Open]
undirectedDfs g = go Set.empty
  where
    go _ [] = []
    go visited (cur:stack) = case Set.member cur visited of
      True  -> go visited stack
      False -> cur : go (Set.insert cur visited) (adjacent g cur ++ stack)

-- | Nodes that are one step away from the given node
adjacent :: OpenHypergraph a -> Wire Open -> [Wire Open]
adjacent g (Port s _, Port t _) =
  uncurry (++) (wires g s) ++ uncurry (++) (wires g t)

-- | Input and output wires of a given hyperedge.
wires
  :: Ord (f HyperEdgeId)
  => Hypergraph f sig
  -> f HyperEdgeId
  -> ([Wire f], [Wire f])
wires g x = (inputs, outputs) where
  inputs  =
    f [ (,Port x i) <$> Bimap.lookupR (Port x i) (connections g) | i <- [0..] ]
  outputs  =
    f [ (Port x i,) <$> Bimap.lookup  (Port x i) (connections g) | i <- [0..] ]
  f = catMaybes . takeWhile isJust
