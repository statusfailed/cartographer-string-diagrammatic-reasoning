{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Hypergraph.Test.Arbitrary where

import Control.Monad
import Data.Semigroup
import Data.Hypergraph as Hypergraph

import Debug.Trace

import Test.QuickCheck

instance (Signature sig, Arbitrary sig) => Arbitrary (OpenHypergraph sig) where
  arbitrary = sized generateSized

-- | Generate a hypergraph with a specific number of generators.
generateSized
  :: (Signature sig, Arbitrary sig) => Int -> Gen (OpenHypergraph sig)
generateSized 0 = return Hypergraph.empty -- show me the base case
generateSized 1 = Hypergraph.singleton <$> arbitrary -- I said, the REAL base case.
-- In the general case, split n at a random point (1..n-1), then generate two
-- sub-hypergraphs of size (k < n, n - k).
generateSized n = do
  k <- choose (1,n-1)

  a <- generateSized k
  b <- generateSized (n - k)

  {-return . trace ("a: " ++ show a) . trace ("b: " ++ show b) $ (a <> b)-}
  {-return (a → b)-}
  isCompose <- arbitrary
  return (if isCompose then a → b else a <> b)

-------------------------------
-- A Generator type for use in testing.

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

