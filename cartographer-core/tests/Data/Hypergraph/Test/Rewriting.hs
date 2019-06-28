module Data.Hypergraph.Test.Rewriting where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.Hypergraph
import Data.Hypergraph.Test.Arbitrary

import Data.List (sort)
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Map

import Debug.Trace

-------------------------------
-- Tests

tests = testGroup "Data.Hypergraph.Rewrite"
  [ QC.testProperty "prop_rewriteSelfIdentity" prop_rewriteSelfIdentity
  , QC.testProperty "prop_rewriteSelfSelf" prop_rewriteSelfSelf
  ]

-- Take integers (k, n) and an OpenHypergraph of size (k', n'), and return a
-- new hypergraph of size (k, n) by prepending and appending generators of size
-- (k, k') and (n', n), respectively.
adaptSize :: Int -> Int -> OpenHypergraph Generator -> OpenHypergraph Generator
adaptSize k n g = a → g → b
  where
    (k', n') = toSize g
    a = singleton $ Generator 0 (k, k')
    b = singleton $ Generator 0 (n', n)

-------------------------------
-- Properties

-- | Given an arbitrary k → n graph, attach a 1 → k generator to the front, and
-- a n → 1 generator to the end, and rewrite to the identity wire.
prop_rewriteSelfIdentity :: OpenHypergraph Generator -> Property
prop_rewriteSelfIdentity g =
  let h = adaptSize 1 1 g
      m = head (match h h)
      r = rewrite m identity h
  in  r { nextHyperEdgeId = 0 } === identity

-- Rewriting a graph to itself should result in a graph which matches itself.
prop_rewriteSelfSelf :: OpenHypergraph Generator -> Bool
prop_rewriteSelfSelf g =
  let m = head (match g g)
      r = rewrite m g g
  in  (not . Prelude.null) (match g r :: [Matching Generator])
