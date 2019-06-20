module Data.Hypergraph.Test
  ( module Data.Hypergraph
  , module Data.Hypergraph.Test.Arbitrary
  , module Type
  , mainTests
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.Hypergraph
import Data.Hypergraph.Test.Arbitrary
import Data.Hypergraph.Test.Type as Type
import Data.Hypergraph.Test.Match as Match

mainTests :: TestTree
mainTests = testGroup "Data.Hypergraph"
  [ Type.tests
  , Match.tests
  ]

main :: IO ()
main = defaultMain mainTests
