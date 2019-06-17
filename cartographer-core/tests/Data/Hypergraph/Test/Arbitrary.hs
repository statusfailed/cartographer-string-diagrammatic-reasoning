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
