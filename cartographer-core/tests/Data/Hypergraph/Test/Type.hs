{-# LANGUAGE PartialTypeSignatures #-}
module Data.Hypergraph.Test.Type where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

-- SUT
import Data.Hypergraph.Type

tests = testGroup "Data.Hypergraph.Type"
  [ QC.testProperty "foo" prop_foo ]

-- | The nextHyperEdgeId field should be larger than any hyperedge appearing in
-- any of the connections or edge types.
prop_nextHyperEdgeIdSupremum :: OpenHypergraph sig -> Bool
prop_nextHyperEdgeIdSupremum g = undefined

prop_foo :: Int -> Bool
prop_foo i = i == i

-------------------------------
-- TODO: put these in Unsafe module tests.

-- | Incrementing hyperedge IDs means none are smaller than the increment value.
prop_incrementHyperEdgeIdMinimum :: OpenHypergraph sig -> Bool
prop_incrementHyperEdgeIdMinimum = undefined
