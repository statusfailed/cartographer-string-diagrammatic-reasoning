{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module Main where

import Criterion.Main
import Control.Monad.Logic

import Data.Hypergraph
import Data.Time.Clock
import Data.List (foldl')
import Data.Maybe (fromJust)

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import GHC.Generics
import Control.DeepSeq

data Generator = Generator (Int, Int)
  deriving(Eq, Ord, Read, Show, Generic, NFData)

instance Signature Generator where
  toSize (Generator x) = x

-- force evaluation of both maps in the hypergraph.
graphSize :: OpenHypergraph sig -> (Int, Int)
graphSize g = (Bimap.size (connections g), Map.size (signatures g))

-- force evaluation of a matching by counting matched wires and edges
matchingSize :: Matching a -> (Int, Int)
matchingSize (Matching wires edges) = (Bimap.size wires, Bimap.size edges)

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

main = do
  defaultMain
    [ bgroup "construct"
      [ bench "constructThin" $ nf constructThin 10000
      , bench "constructWide" $ nf constructWide 10000
      , bench "constructThinAffine" $ nf constructWide 10000
      ]
    , bgroup "match"
      [ bgroup "rare"
        [ bgroup "compose"
          [ surroundBench "noPartials" size (→) (copy → add) bit
          , surroundBench
              "manyPartials" size (→) (add → copy) (add → bit → copy)
          ]
        , bgroup "tensor"
          [ surroundBench "noPartials" size (<>) (copy → add) bit
          , surroundBench
              "manyPartials" size (<>) (add → copy) (add → bit → copy)
          ]
        ]
      , bgroup "bigPattern"
        [ let pattern = foldl' (→) empty (replicate size bit)
          in  surroundBench "bit" size (→) (copy → add) pattern
        ]
      , bgroup "homogenous"
        [ homogenous "bit" (size*10) bit
        , homogenous "coherence" (size*10) coherence
        ]
      ]
    ]
  where
    size = 10000

-- Time to find the first match of a somewhat complicated pattern in a graph
-- where that pattern is extremely common.
homogenous :: String -> Int -> OpenHypergraph Generator -> Benchmark
homogenous name size pattern =
  let g = foldl' (→) empty (replicate size pattern)
  in  bench name $ nf (head . uncurry match) (pattern,g)

-- construct a graph using size*2 repetitions of 'filler' where 'pattern' only
-- occurs once (sandwiched between the fillers)
surround
  :: (OpenHypergraph a -> OpenHypergraph a -> OpenHypergraph a) -- ^ function to compose graphs
  -> Int -- ^ size of filler on each side
  -> OpenHypergraph a -- ^ filler graph
  -> OpenHypergraph a -- ^ pattern to occur once
  -> OpenHypergraph a
surround (·) size filler pattern = g · pattern · g
  where g = foldl' (·) empty (replicate size filler)

surroundBench
  :: (a ~ Generator)
  => String
  -> Int
  -> (OpenHypergraph a -> OpenHypergraph a -> OpenHypergraph a)
  -> OpenHypergraph a
  -> OpenHypergraph a
  -> Benchmark
surroundBench name size (·) filler pattern =
  let g = surround (<>) size filler pattern
  in  bench name $ nf (head . uncurry match) (pattern, g)

-------------------------------
-- Various graphs for benching with

add = singleton (Generator (2,1))
bit = singleton (Generator (1,1))
copy = singleton (Generator (1,2))

coherence = (copy <> copy) → middle → (add <> add)
  where
    -- the "coherence" subgraph
    middle = fromJust $ permute [0,2,1,3]
