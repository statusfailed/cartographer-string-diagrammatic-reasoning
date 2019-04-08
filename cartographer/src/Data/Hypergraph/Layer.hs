{-# LANGUAGE TupleSections #-}
module Data.Hypergraph.Layer where

import Data.Maybe (catMaybes)
import Control.Monad

import Data.Hypergraph.Type
import Data.Hypergraph.Traversal (bfs, bfsAcyclic)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | Assign to each Hyperedge its distance from the left boundary.
-- As long as the 'Hypergraph' is acyclic, this is a valid layering function.
layer
  :: Signature sig
  => OpenHypergraph sig
  -> [(HyperEdgeId, Int)]
layer = uncurry toEdges <=< layer'

layer'
  :: Signature sig
  => OpenHypergraph sig
  -> [(Wire Open, Int)]
layer' hg = uncurry f =<< zip (bfsAcyclic hg) [0..]
  where
    f wires i = fmap (,i) wires

toEdges :: Wire Open -> Int -> [(HyperEdgeId, Int)]
toEdges (s, t) i =
  catMaybes $ [ (,i) <$> toHyperEdgeId s, (,i+1) <$> toHyperEdgeId t ]

toHyperEdgeId :: Port a Open -> Maybe HyperEdgeId
toHyperEdgeId (Port Boundary _) = Nothing
toHyperEdgeId (Port (Gen e) _)  = Just e
