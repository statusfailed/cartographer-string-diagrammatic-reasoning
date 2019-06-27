{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Algebraically construct hypergraphs as monoidal categories.
module Data.Hypergraph.Algebraic
  ( (→)
  , tensor
  , permute
  , identityN
  , swap
  , OpenHypergraph(..)
  ) where

import Prelude hiding (id, (.))
import Control.Monad
import Control.Category
import Control.Arrow
import Data.Monoid
import Data.Maybe (catMaybes, isJust)
import Data.List (foldl', sort)
import Data.Reflection

import Data.Hypergraph.Type as Hypergraph

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

instance Semigroup (OpenHypergraph sig) where
  (<>) = tensor

instance Monoid (OpenHypergraph sig) where
  mempty = Hypergraph.empty

tensor :: OpenHypergraph sig -> OpenHypergraph sig -> OpenHypergraph sig
tensor a b = a
  { connections = foldl' (flip $ uncurry Bimap.insert) (connections a) newWires
  , signatures  = foldl' (flip $ uncurry Map.insert) (signatures a) newEdges
  , nextHyperEdgeId = maxA + maxB
  }
  where
    newEdges = (\(e,s) -> (e + maxA, s)) <$> Map.toList (signatures b)
    newWires = (fixPort *** fixPort) <$> Bimap.toList (connections b)

    maxA = nextHyperEdgeId a
    maxB = nextHyperEdgeId b
    (ai, ao) = hypergraphSize a
    (bi, bo) = hypergraphSize b
    offset   = max 0 (ao - bi)

    fixPort :: Reifies a PortRole => Port a Open -> Port a Open
    fixPort p@(Port Boundary i) = Port Boundary (i + offset)
      where offset = portRole ai ao p
    fixPort (Port (Gen e) i) = Port (Gen $ e + maxA) i


-- | Sequentially compose two hypergraphs, even when types don\'t match.
-- Wires left dangling as a result of mismatched types will automatically be
-- connected to their corresponding boundary.
--
-- in a → b, if a has more outputs, then b will connect to the *lowermost*
-- outputs of a. If b has more inputs than a has outputs, then a will connect
-- to the *uppermost* inputs of b, e.g.:
--
-- This is "asymmetric" because it means no changes ever have to be made to the
-- "a" graph, which is assumed to be much larger than the "b" graph.
--
--   ┌───┐    ┌───┐           ┌───┐
--   │ A │────│ B │           │ A │─────────
--   └───┘    │   │           │   │     ┌───┐
--   ─────────│   │           │   │──── │ B │
--            └───┘           └───┘     └───┘
--   A has fewer outputs       B has fewer inputs
--
-- see wiki/ALGEBRAIC.md for implementation details
--
-- NOTE: this is just a special case of rewriting, where the match is every
-- wire connected to the RHS boundary, plus a little extra work to make it
-- "affine".
-- It might be better to reimplement this, but I think handling as a special
-- case makes it a bit faster?
(→) :: OpenHypergraph a -> OpenHypergraph a -> OpenHypergraph a
a → b = a
  { connections = foldl' (flip $ uncurry Bimap.insert) (connections a) newWires
  , signatures  = foldl' (flip $ uncurry Map.insert) (signatures a) newEdges
  , nextHyperEdgeId = maxA + maxB
  }
  where
    newEdges = (\(e,s) -> (e + maxA, s)) <$> Map.toList (signatures b)
    newWires = rewireB <$> Bimap.toList (connections b)

    maxA = nextHyperEdgeId a
    maxB = nextHyperEdgeId b
    (ai, ao) = hypergraphSize a
    (bi, bo) = hypergraphSize b
    offset   = max 0 (ao - bi)

    rewireB :: Wire Open -> Wire Open
    rewireB = onFst reindexLeft . pairUp . (reindexPort *** reindexPort)
      where onFst f (a,b) = (f a, b)

    -- TODO: don't bother looking up if i >= ao.
    pairUp :: Wire Open -> Wire Open
    pairUp w@(Port Boundary i, t) =
      case Bimap.lookupR (Port Boundary i) (connections a) of
        Nothing -> w
        Just s' -> (s', t)
    pairUp w = w

    -- OK, good.
    reindexPort (Port Boundary i) = Port Boundary (i + offset)
    reindexPort (Port (Gen e)  i) = Port (Gen (e + maxA)) i

    -- NOTE: only called *after* matchBoundaries, so it will only get ports
    -- which will eventually connect to the boundary.
    reindexLeft (Port Boundary i) = Port Boundary (i - ao + ai)
    reindexLeft p = p

-- | Create a permutation of wires. If the supplied argument is not a
-- permutation, Nothing is returned.
--
-- TODO: test me!
permute :: [Int] -> Maybe (OpenHypergraph a)
permute ports
  | isPermutation ports =
      Just $ Hypergraph
        { connections = Bimap.fromList (toWire <$> zip [0..] ports)
        , signatures = Map.empty
        , nextHyperEdgeId = 0
        }
  | otherwise = Nothing
  where
    toWire (i,j) = (Port Boundary i, Port Boundary j)
    isPermutation = all (uncurry (==)) . zip [0..] . sort

-- | the identity wire, tensored together n times.
-- NOTE: uses unsafe fromJust.
identityN :: Int -> OpenHypergraph a
identityN n = case permute [0..n-1] of
  Just g -> g
  Nothing -> error "identityN (impossibly) failed"

-- | Swap "bundles" of wires
-- /swap k n/ creates a (k + n → k + n) graph, where the first k inputs connect
-- to the last k outputs, and the last n inputs connect to the first n outputs.
--
-- for example, swap 1 1 == twist, and swap 2 1 is rendered below:
--
--  ────┐
--  ──┐┌┼───
--  ──┼┘└───
--    └─────
--
swap :: Int -> Int -> OpenHypergraph a
swap k n = case permute ([n .. m-1] ++ [0..n-1]) of
  Just g -> g
  Nothing -> error "swap (impossibly) failed"
  where m = k + n
