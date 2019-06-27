module Main where

import Criterion.Main
import Control.Monad.Logic

import Data.Hypergraph
import Data.Time.Clock
import Data.List (foldl')

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Generator = Generator (Int, Int)
  deriving(Eq, Ord, Read, Show)

instance Signature Generator where
  toSize (Generator x) = x

-- force evaluation of both maps in the hypergraph.
graphSize :: OpenHypergraph sig -> (Int, Int)
graphSize g = (Bimap.size (connections g), Map.size (signatures g))

constructThin :: Int -> (Int, Int)
constructThin n = graphSize r
  where
    a = singleton (Generator (2,1))
    b = singleton (Generator (1,1))
    c = singleton (Generator (1,2))
    r = foldl' (→) empty (replicate n $ c → a)

constructThinAffine :: Int -> (Int, Int)
constructThinAffine n = graphSize r
  where
    a = singleton (Generator (2,1))
    r = foldl' (→) empty (replicate n $ a)

constructWide :: Int -> (Int, Int)
constructWide n = graphSize r
  where
    a = singleton (Generator (2,1))
    b = singleton (Generator (1,1))
    c = singleton (Generator (1,2))
    r = foldl' (<>) empty (replicate n $ c → a)

-- match a singleton sandwiched between two (2­→ 2) graphs.
singletonThinMatch :: Int -> Int
singletonThinMatch n = length . observeAll $ match b g
  where
    a = singleton (Generator (2,1))
    b = singleton (Generator (2,2))
    c = singleton (Generator (1,2))
    r = foldl' (→) empty (replicate n $ c → a)
    g = r → b → r

singletonWideMatch :: Int -> Int
singletonWideMatch n = length . observeAll $ match b g
  where
    a = singleton (Generator (2,1))
    b = singleton (Generator (2,2))
    c = singleton (Generator (1,2))
    r = foldl' (<>) empty (replicate n $ c → a)
    g = r <> b <> r

hugeThinMatch :: Int -> Int
hugeThinMatch n = length . observeAll $ match r g
  where
    a = singleton (Generator (2,1))
    b = singleton (Generator (2,2))
    c = singleton (Generator (1,2))
    r  = foldl' (→) empty (replicate n $ c → a)
    r' = foldl' (→) empty (replicate n b)
    g = r' → r

main = do
  defaultMain
    [ bgroup "construct"
      [ bench "constructThin" $ nf constructThin 10000
      , bench "constructWide" $ nf constructWide 10000
      , bench "constructThinAffine" $ nf constructWide 10000
      ]
    -- TODO: these benchmarks include "build time" for the hypergraphs,
    -- which we don't want.
    , bgroup "match"
      [ bench "singletonThinMatch" $ nf singletonThinMatch 5000
      , bench "singletonWideMatch" $ nf singletonWideMatch 5000
      , bench "hugeThinMatch" $ nf singletonWideMatch 5000
      ]
    ]
