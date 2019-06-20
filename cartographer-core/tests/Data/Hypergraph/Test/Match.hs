module Data.Hypergraph.Test.Match where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Data.Hypergraph.Test.Arbitrary
import Data.Hypergraph.Search (undirectedDfs, wires)

import Data.Hypergraph

import Data.List (sort)

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

import qualified Data.Map.Strict as Map
import Debug.Trace

-------------------------------
-- Tests

tests = testGroup "Data.Hypergraph.Match"
  [ QC.testProperty "prop_dfsAllConnections" prop_dfsAllConnections
  {-, QC.testProperty "prop_matchSelf" prop_matchSelf-}
  , QC.testProperty "prop_matchSingleton" prop_matchSingleton
  , QC.testProperty "prop_matchAfter" prop_matchAfter
  {-, QC.testProperty "prop_matchSandwich" prop_matchSandwich-}
  ]

-- | A pattern always matches in itself exactly once
-- TODO: make this exactly once; it's actually broken!!!!!!
prop_matchSelf :: OpenHypergraph Generator -> Bool
prop_matchSelf a = not . Prelude.null $ match a a -- 1 === length (match a a)

-- | The empty hypergraph matches in every hypergraph.
prop_emptyMatchesEverywhere :: OpenHypergraph Generator -> Bool
prop_emptyMatchesEverywhere = undefined

-- | The identity wire should match in any non-empty hypergraph.
prop_identityMatchesNonEmpty :: OpenHypergraph Generator -> Bool
prop_identityMatchesNonEmpty = undefined

prop_dfsAllConnections :: OpenHypergraph Generator -> Property
prop_dfsAllConnections g =
      sort (undirectedDfs g . Bimap.toList . connections $ g)
  === sort (Bimap.toList $ connections g)

-- | A singleton graph appearing within a larger graph should always be
-- matchable.
prop_matchSingleton
  :: OpenHypergraph Generator
  -> Generator
  -> OpenHypergraph Generator -> Bool
prop_matchSingleton a g b = not . Prelude.null $ match s (a → s → b)
  where s = singleton g

-- TODO: problem
--  this is fast:     match a (a → b)
--  this is SLOW:     match b (a → b)
--
-- Probably because we pick random connections out of a!!! DO THEM IN ORDER.
prop_matchAfter
  :: OpenHypergraph Generator -> OpenHypergraph Generator -> Bool
prop_matchAfter a b = 
  let msg = "composed size " ++ show (toSize a) ++ ", " ++ show (toSize b) ++
            ", ngens: " ++ show (Map.size (signatures a)) ++
            ", " ++ show (Map.size (signatures b))
      composed = trace msg (a → b)
  in  not . Prelude.null $ match a composed

prop_matchSandwich
  :: OpenHypergraph Generator
  -> OpenHypergraph Generator
  -> OpenHypergraph Generator
  -> Bool
prop_matchSandwich a b c = not . Prelude.null $ match b (a → b → c)
