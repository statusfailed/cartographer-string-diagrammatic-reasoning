{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Algebraically construct hypergraphs as monoidal categories.
module Data.Hypergraph.Algebraic
  ( (→)
  , (⇒)
  , tensor
  , OpenHypergraph(..)
  , mapSourceBoundaryMonotonic
  , mapTargetBoundaryMonotonic
  , rewire
  , rewireRightWire
  , rewireLeftWire
  {-, dual-}
  ) where

import Prelude hiding (id, (.))
import Control.Monad
import Control.Category
import Control.Arrow
import Data.Monoid
import Data.Maybe (catMaybes, isJust)
import Data.List (foldl')

import Data.Hypergraph.Type as Hypergraph
import Data.Hypergraph.Unsafe (incrementHyperEdgeIds, mapPortEdge, mergeR)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

import Debug.Trace

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
(⇒) :: OpenHypergraph a -> OpenHypergraph a -> OpenHypergraph a
a ⇒ bOld = Hypergraph cs ss (nextHyperEdgeId b)
  where
    -- Renumber hyperedge Ids.
    -- log-linear in b' (rebuilds the entire connection bimap)
    b = incrementHyperEdgeIds (nextHyperEdgeId a) bOld
    -- connections of b, but boundaries replaced with IDs of generators in a.
    bcs = foldl' (flip $ uncurry Bimap.insert) (connections b) (rewire a b)

    ss = Map.union (signatures a) (signatures b)

    cs = foldl' (flip$ uncurry Bimap.insert) (connections a) (Bimap.toList bcs)

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

-- TODO: Could this be very slow? Recomputing size each time is kinda dumb!
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

-------------------------------
-- reimplementation

-- We have to do these things
--  * update (offset) right-boundary wires of a
--  * update (offset) left-boundary wires of b
--  * add (nextHyperEdgeId a) to wires and edges of b
--  * set left boundaries of b to source of right boundary in a
(→) :: OpenHypergraph a -> OpenHypergraph a -> OpenHypergraph a
a → b = a
  { connections = foldl' (flip $ uncurry Bimap.insert) (connections a) newWires
  , signatures  = foldl' (flip $ uncurry Map.insert) (signatures a) newEdges
  , nextHyperEdgeId = maxA + maxB
  }
  where
    (ai, ao) = hypergraphSize a
    (bi, bo) = hypergraphSize b
    maxA = nextHyperEdgeId a
    maxB = nextHyperEdgeId b

    awires = rewireLeft a (bi, bo)
    bwires = rewireRightWire (ai, ao) . fixRightWire a b . renumber
          <$!> Bimap.toList (connections b)

    renumber (s, t) = (mapPortEdge (+maxA) s, mapPortEdge (+maxA) t)

    newEdges = (\(e,s) -> (e + maxA, s)) <$> Map.toList (signatures b)
    newWires = awires ++ bwires

-- Get the affine-offset wires of the left hypergraph in a composition
rewireLeft :: OpenHypergraph a -> (Int, Int) -> [Wire Open]
rewireLeft a (bi, bo) = go bi where
  go i = case Bimap.lookupR (Port Boundary i) (connections a) of
    Just s  -> rewireLeftWire (bi, bo) (s, Port Boundary i) : go (succ i)
    Nothing -> []

-- | Fix a wire in the left side of affineCompose
rewireLeftWire :: (Int, Int) -> Wire Open -> Wire Open
rewireLeftWire (bi, bo) (s, Port Boundary i)
  = if i >= bi then (s, Port Boundary (i - bi + bo)) else (s, Port Boundary i)
rewireLeftWire _ w = w

-- | Fix a wire on the RIGHT side of affineCompose
rewireRightWire (ai, ao) (Port Boundary i, t)
  = if i >= ao then (Port Boundary (i - ao + ai), t) else (Port Boundary i, t)
rewireRightWire _ w = w

-- Modify boundary wires from the latter hypergraph so that
fixRightWire :: OpenHypergraph a -> OpenHypergraph a -> Wire Open -> Wire Open
fixRightWire a b w@(Port Boundary i, t)
  = maybe w (\s -> (s,t)) $! Bimap.lookupR (Port Boundary i) (connections a)
fixRightWire a b w = w
