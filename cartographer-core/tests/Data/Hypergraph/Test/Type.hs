{-# LANGUAGE PartialTypeSignatures #-}
module Data.Hypergraph.Test.Type where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import qualified Data.Map.Strict as Map
import qualified Data.Bimap as Bimap

import Data.Hypergraph.Test.Arbitrary

import Data.Hypergraph
import Data.Hypergraph.Traversal

-- | A test signature to try and capture many behaviours
data Generator = Generator
  { generatorFlag :: Int
  , generatorType :: (Int, Int)
  } deriving(Eq, Ord, Read, Show)

instance Signature Generator where
  toSize = generatorType

-- | Generate small signatures, with 2 generators of each type from (
instance Arbitrary Generator where
  arbitrary = Generator <$> choose (0, 1) <*> randomType
    where
      randomType = do
        k <- choose (1, 4) -- total number of ports
        n <- choose (0, k) -- of which n are inputs
        return (n, k - n)

-------------------------------
-- Tests

tests = testGroup "Data.Hypergraph.Type"
  [ QC.testProperty "prop_singletonSize" prop_singletonSize
  , QC.testProperty "prop_portsHaveConnections" prop_portsHaveConnections
  , QC.testProperty "prop_affineCompositionSize" prop_affineCompositionSize
  , QC.testProperty "prop_monoidalProductType" prop_monoidalProductSize
  ]

-- | Every port in a properly constructed 'OpenHypergraph' should be connected
-- to another.
prop_portsHaveConnections :: OpenHypergraph Generator -> Bool
prop_portsHaveConnections a = all portsConnected (Map.toList $ signatures a)
  where
    cs = connections a
    portsConnected (e, g) = sourcePortsConnected e g && targetPortsConnected e g
    sourcePortsConnected e g = all (flip Bimap.member cs)  (sourcePorts e g)
    targetPortsConnected e g = all (flip Bimap.memberR cs) (targetPorts e g)

-- | Every connection in the hypergraph refers to a real port (i.e., the
-- HyperEdgeId has a corresponding signature, and the port index is less than
-- or equal to that generator\'s size.)
prop_connectionsHavePorts :: OpenHypergraph Generator -> Bool
prop_connectionsHavePorts = undefined

-- | The size of a "singleton" hypergraph is the same size as the one generator
-- in that graph.
prop_singletonSize :: Generator -> Property
prop_singletonSize a = toSize a === toSize (singleton a)

-- | The "affine composition" operator
prop_affineCompositionSize
  :: OpenHypergraph Generator -> OpenHypergraph Generator -> Property
prop_affineCompositionSize a b =
  toSize (a → b) === (i + max 0 (m - n), o + max 0 (n - m))
  where (i, n) = toSize a
        (m, o) = toSize b

-- | The nextHyperEdgeId field should be larger than any hyperedge appearing in
-- any of the connections or edge types.
-- prop_nextHyperEdgeIdSupremum :: OpenHypergraph sig -> Bool
-- prop_nextHyperEdgeIdSupremum g = undefined

-- | Monoidal product of types (n → m) and (n' → m') should give
-- type (n + n' → m + m')
prop_monoidalProductSize
  :: OpenHypergraph Generator -> OpenHypergraph Generator -> Bool
prop_monoidalProductSize a b =
  toSize (a <> b) == toSize a +++ toSize b
  where (a,b) +++ (c,d) = (a+c, b+d)

-------------------------------
-- TODO: put these in Unsafe module tests.

-- | Incrementing hyperedge IDs means none are smaller than the increment value.
-- prop_incrementHyperEdgeIdMinimum :: OpenHypergraph sig -> Bool
-- prop_incrementHyperEdgeIdMinimum = undefined
