module Data.Hypergraph.Test
  ( module Data.Hypergraph
  , module Type
  , mainTests
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.Hypergraph
import Data.Hypergraph.Test.Type as Type

mainTests :: TestTree
mainTests = testGroup "Data.Hypergraph"
  [ Type.tests ]

main :: IO ()
main = defaultMain mainTests
