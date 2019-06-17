{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Algebraically construct hypergraphs as monoidal categories.
module Data.Hypergraph.Algebraic
  ( (→)
  , tensor
  , OpenHypergraph(..)
  , mapSourceBoundaryMonotonic
  , mapTargetBoundary
  , incrementHyperEdgeIds
  , reconnectSource
  {-, dual-}
  ) where

import Prelude hiding (id, (.))
import Control.Category
import Data.Monoid
import Data.Maybe (catMaybes)

import Data.Hypergraph.Type as Hypergraph
import Data.Hypergraph.Unsafe (incrementHyperEdgeIds, mergeR)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

newtype SMC sig a b = SMC { runSMC :: OpenHypergraph sig }

instance Semigroup (OpenHypergraph sig) where
  (<>) = tensor

instance Monoid (OpenHypergraph sig) where
  mempty = Hypergraph.empty

instance Category (SMC sig) where
  id  = SMC Hypergraph.identity
  SMC r . SMC l = SMC (l → r)

-- | Composition of two hypergraphs
--
-- 'compose a b' connects the right boundary of a to the left boundary of b.
-- 
-- If boundary sizes do not match up, the larger side will be connected to the
-- leftmost or rightmost boundary.
--compose :: OpenHypergraph sig -> OpenHypergraph sig -> OpenHypergraph sig
--compose l r =
--  where
--    lbs = [ Port Boundary i | i <- [0..] ]

-- | Append "b" to the right boundary of "a".
-- (log-linear in size of b?)
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

    ss = Map.union (signatures a) (signatures b)

    cs = foldr (uncurry Bimap.insert) (connections a) $
      reconnectSource a <$> Bimap.toList (connections b)

-- | Monoidal product (×) of two hypergraphs.
tensor :: OpenHypergraph sig -> OpenHypergraph sig -> OpenHypergraph sig
tensor a b' = a `mergeR` b where
  (n, m) = hypergraphSize a
  b = mapSourceBoundaryMonotonic (+ n) -- ^ Boundary ports start from n
    . mapTargetBoundaryMonotonic (+ m) -- ^ Boundary ports start from m
    . incrementHyperEdgeIds (nextHyperEdgeId a)
    $ b'

-- | Map a monotonic function over the indexes of source boundary ports in an
-- 'OpenHypergraph'
mapSourceBoundaryMonotonic
  :: (Int -> Int) -> OpenHypergraph sig -> OpenHypergraph sig
mapSourceBoundaryMonotonic f hg
  = hg { connections = Bimap.mapMonotonic g (connections hg) }
  where
    g (Port Boundary i) = Port Boundary (f i)
    g p = p

mapTargetBoundaryMonotonic
  :: (Int -> Int) -> OpenHypergraph sig -> OpenHypergraph sig
mapTargetBoundaryMonotonic f hg
  = hg { connections = Bimap.mapMonotonicR g (connections hg) }
  where
    g (Port Boundary i) = Port Boundary (f i)
    g p = p

-- | Flip all the arrows
dual :: OpenHypergraph sig -> OpenHypergraph sig
dual = undefined

-------------------------------
-- Utilities

-- | Given a Wire, if the source is a left boundary, replace it with the source
-- of the right boundary at the same index appearing in the supplied
-- 'OpenHypergraph'
reconnectSource
  :: OpenHypergraph sig
  -> Wire Open
  -> Wire Open
reconnectSource a (s, t) = (s', t) where
  s' = case s of
    p@(Port Boundary i) ->
      maybe p id (Bimap.lookupR (Port Boundary i) (connections a))
    p -> p
