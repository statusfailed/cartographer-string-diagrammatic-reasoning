module Data.Hypergraph.Test where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import qualified Data.Hypergraph.Test.Type as Type

tests :: TestTree
tests = testGroup "Data.Hypergraph"
  [ Type.tests ]
