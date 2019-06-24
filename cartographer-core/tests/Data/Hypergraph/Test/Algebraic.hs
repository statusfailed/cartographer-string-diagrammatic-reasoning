-- | Test construction of hypergraphs
module Data.Hypergraph.Test.Algebraic where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.List (sort)
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

import Data.Hypergraph
import Data.Hypergraph.Test.Arbitrary
import Data.Hypergraph.Search (undirectedDfs)

-------------------------------
-- Tests

tests = testGroup "Data.Hypergraph.Algebraic"
  [ QC.testProperty "prop_dfsAllConnections" prop_dfsAllConnections
  , QC.testProperty "prop_complete" prop_complete
  , QC.testProperty "prop_completeSingletons" prop_completeSingletons
  ]

-- | Verify that composing two singleton hypergraphs is Complete.
-- This test is redundant, but makes debugging much easier!
prop_completeSingletons :: Generator -> Generator -> Bool
prop_completeSingletons a b = isComplete (singleton a → singleton b)

-- | Every hypergraph is complete
prop_complete :: OpenHypergraph Generator -> Bool
prop_complete a = isComplete a

-- | undirectedDfs visits all nodes in the hypergraph
prop_dfsAllConnections :: OpenHypergraph Generator -> Property
prop_dfsAllConnections g =
      sort (undirectedDfs g . Bimap.toList . connections $ g)
  === sort (Bimap.toList $ connections g)
