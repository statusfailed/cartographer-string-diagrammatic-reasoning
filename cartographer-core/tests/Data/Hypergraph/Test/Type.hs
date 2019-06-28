{-# LANGUAGE PartialTypeSignatures #-}
module Data.Hypergraph.Test.Type where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import qualified Data.Map.Strict as Map
import qualified Data.Bimap as Bimap

import Data.Hypergraph.Test.Arbitrary 
import Data.Hypergraph
import Data.Hypergraph.Traversal

-------------------------------
-- Tests

tests = testGroup "Data.Hypergraph.Type"
  [ QC.testProperty "prop_singletonSize" prop_singletonSize
  , QC.testProperty "prop_isComplete" prop_isComplete
  , QC.testProperty "prop_portsHaveConnections" prop_portsHaveConnections
  , QC.testProperty "prop_connectionsHavePorts" prop_connectionsHavePorts 
  , QC.testProperty "prop_affineCompositionSize" prop_affineCompositionSize
  , QC.testProperty "prop_monoidalProductSize" prop_monoidalProductSize
  , testGroup "duals"
    [ QC.testProperty "prop_dualSwapsSize" prop_dualSwapsSize
    , QC.testProperty "prop_dualPreservesNumberOfEdges"
        prop_dualPreservesNumberOfEdges
    , QC.testProperty "prop_dualPreservesNumberOfWires"
        prop_dualPreservesNumberOfWires
    ]
  ]

prop_isComplete :: OpenHypergraph Generator -> Bool
prop_isComplete = isComplete

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
-- the generator\'s size.)
prop_connectionsHavePorts :: OpenHypergraph Generator -> Bool
prop_connectionsHavePorts g = all hasPorts (Bimap.toList $ connections g) where
  hasPorts (source, target) = hasSource source && hasTarget target
  hasSource (Port Boundary _) = True
  hasSource (Port (Gen x) i) =
    maybe False ((>i) . snd . toSize) (signatureOf x g)

  hasTarget (Port Boundary _) = True
  hasTarget (Port (Gen x) i) =
    maybe False ((>i) . fst . toSize) (signatureOf x g)

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

-- | Monoidal product of types (n → m) and (n' → m') should give
-- type (n + n' → m + m')
prop_monoidalProductSize
  :: OpenHypergraph Generator -> OpenHypergraph Generator -> Bool
prop_monoidalProductSize a b =
  toSize (a <> b) == toSize a +++ toSize b
  where (a,b) +++ (c,d) = (a+c, b+d)

-------------------------------
-- test Duals

prop_dualSwapsSize :: OpenHypergraph Generator -> Property
prop_dualSwapsSize g = toSize g === (swap . toSize . dual opposite) g
  where
    swap (x,y) = (y, x)

prop_dualPreservesNumberOfEdges :: OpenHypergraph Generator -> Property
prop_dualPreservesNumberOfEdges g =
  let n = Map.size (signatures g)
      k = Map.size (signatures $ dual opposite g)
  in  n === k

prop_dualPreservesNumberOfWires :: OpenHypergraph Generator -> Property
prop_dualPreservesNumberOfWires g =
  let n = Bimap.size (connections g)
      k = Bimap.size (connections $ dual opposite g)
  in  n === k
