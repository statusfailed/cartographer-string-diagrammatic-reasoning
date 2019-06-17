{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Algebraically construct hypergraphs as monoidal categories.
module Data.Hypergraph.Algebraic
  ( (→)
  , tensor
  , OpenHypergraph(..)
  , mapSourceBoundaryMonotonic
  , mapTargetBoundaryMonotonic
  , rewire
  {-, dual-}
  ) where

import Prelude hiding (id, (.))
import Control.Category
import Data.Monoid
import Data.Maybe (catMaybes, isJust)

import Data.Hypergraph.Type as Hypergraph
import Data.Hypergraph.Unsafe (incrementHyperEdgeIds, mergeR)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

instance Semigroup (OpenHypergraph sig) where
  (<>) = tensor

instance Monoid (OpenHypergraph sig) where
  mempty = Hypergraph.empty

-- | Sequentially compose two hypergraphs, even when types don\'t match.
-- Wires left dangling as a result of mismatched types will automatically be
-- connected to their corresponding boundary.
--
-- see wiki/ALGEBRAIC.md for implementation details
--
-- NOTE: this is just a special case of rewriting, where the match is every
-- wire connected to the RHS boundary, plus a little extra work to make it
-- "affine". 
-- It might be better to reimplement this, but I think handling as a special
-- case makes it a bit faster?
(→) :: OpenHypergraph a -> OpenHypergraph a -> OpenHypergraph a
a → bOld = Hypergraph cs ss (nextHyperEdgeId b)
  where
    -- Renumber hyperedge Ids.
    -- log-linear in b' (rebuilds the entire connection bimap)
    b = incrementHyperEdgeIds (nextHyperEdgeId a) bOld
    -- connections of b, but boundaries replaced with IDs of generators in a.
    bcs = foldr (uncurry Bimap.insert) (connections b) (rewire a b)

    ss = Map.union (signatures a) (signatures b)

    cs = foldr (uncurry Bimap.insert) (connections a) (Bimap.toList bcs)

-- | Monoidal product (×) of two hypergraphs.
tensor :: OpenHypergraph sig -> OpenHypergraph sig -> OpenHypergraph sig
tensor a b' = a `mergeR` b where
  (n, m) = hypergraphSize a
  b = mapSourceBoundaryMonotonic (+ n) -- ^ Boundary ports start from n
    . mapTargetBoundaryMonotonic (+ m) -- ^ Boundary ports start from m
    . incrementHyperEdgeIds (nextHyperEdgeId a)
    $ b'

-------------------------------
-- Utilities

-- | Map a monotonic function over the indexes of source boundary ports in an
-- 'OpenHypergraph'
mapSourceBoundaryMonotonic
  :: (Int -> Int) -> OpenHypergraph sig -> OpenHypergraph sig
mapSourceBoundaryMonotonic f hg
  = hg { connections = Bimap.mapMonotonic g (connections hg) }
  where
    g (Port Boundary i) = Port Boundary (f i)
    g p = p

-- | Same as mapSourceBoundaryMonotonic, but for target boundary.
mapTargetBoundaryMonotonic
  :: (Int -> Int) -> OpenHypergraph sig -> OpenHypergraph sig
mapTargetBoundaryMonotonic f hg
  = hg { connections = Bimap.mapMonotonicR g (connections hg) }
  where
    g (Port Boundary i) = Port Boundary (f i)
    g p = p

rewire :: OpenHypergraph sig -> OpenHypergraph sig -> [Wire Open]
rewire a b = go 0
  where
    (ai, ao) = hypergraphSize a
    (bi, bo) = hypergraphSize b

    go i =
      let s = Bimap.lookupR (Port Boundary i) (connections a)
          t = Bimap.lookup  (Port Boundary i) (connections b)
      in  case (s, t) of
        (Nothing, Nothing) -> []
        (Just s, Just t)   -> (s, t) : go (succ i)
        (Just s, Nothing)  -> (s, Port Boundary (bo + i - bi)) : go (succ i)
        (Nothing, Just t)  -> (Port Boundary (ai + i - ao), t) : go (succ i)
