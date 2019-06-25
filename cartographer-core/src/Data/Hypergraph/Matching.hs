{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE ScopedTypeVariables    #-}
module Data.Hypergraph.Matching where

import Control.Monad
import Control.Monad.Logic
import Control.Monad.Logic.Class
import Control.Applicative hiding (empty)
import qualified Control.Applicative as A

import Data.Hypergraph.Type as Hypergraph hiding (empty)
import Data.Hypergraph.Search (undirectedDfs)

import Data.List (foldl')
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | Nondeterministically choose an item from the list.
choice :: MonadLogic f => [a] -> f a
choice = foldl' interleave mzero . fmap pure

data Matching a = Matching
  { _matchingWires :: Bimap (Wire a Open) (Wire a Open)
  , _matchingEdges :: Bimap HyperEdgeId HyperEdgeId
  } deriving(Eq, Ord, Show)

empty :: Matching a
empty = Matching Bimap.empty Bimap.empty

matchAll
  :: (Eq a, Ord a)
  => OpenHypergraph a -> OpenHypergraph a -> [Matching a]
matchAll pattern context = observeAll $ match pattern context

-- NOTE: it seems about 500x faster in certain cases to just use list, and
-- avoid the 'choice' function. Not sure why this is,
match
  :: (Eq a, Ord a, MonadLogic f)
  => OpenHypergraph a -> OpenHypergraph a -> f (Matching a)
match pattern context
  | Hypergraph.null pattern = pure empty -- empty pattern => empty matching
  | otherwise = foldM (matchWire pattern context) empty wires
  where
    wires = undirectedDfs pattern (Bimap.toList $ connections pattern)

-- | Match a wire from the pattern with a wire in the context.
-- First proposes possible matches (candidates), then checks the match would be
-- consistent, then updates the matching.
matchWire
  :: (Eq a, Ord a, MonadLogic f)
  => OpenHypergraph a
  -> OpenHypergraph a
  -> Matching a -> Wire a Open -> f (Matching a)
matchWire pattern context m w = do
  cw <- candidates pattern context m w
  guard (consistent pattern context m w cw)
  pure (update pattern context m w cw)

-- Try to add a pairing of wires to the matching. New hyperedges may also be
-- added to the matching.
update
  :: Ord a
  => OpenHypergraph a
  -> OpenHypergraph a
  -> Matching a
  -> Wire a Open
  -> Wire a Open
  -> Matching a
update pattern context m pw@(p,q) cw@(p',q') = addEdges . addWires $ m
  where
    addWires x = x { _matchingWires = Bimap.insert pw cw . _matchingWires $ x }
    addEdges = matchPorts q q' . matchPorts p p'

-- | Equate the HyperEdgeIds of two ports in the Matching.
matchPorts :: Port sig a Open -> Port sig a Open -> Matching sig -> Matching sig
matchPorts (Port Boundary _) _ m = m
matchPorts _ (Port Boundary _) m = m
matchPorts (Port (Gen (_,a)) _) (Port (Gen (_,b)) _) m =
  m { _matchingEdges = Bimap.insert a b (_matchingEdges m) }

-- | What are the possible matches for a given wire?
-- We try to exploit the "determined structure" of the hypergraph, by
-- checking if either of the source/target hyperedges is already matched.
candidates
  :: (Eq a, Ord a, MonadLogic f)
  => OpenHypergraph a
  -> OpenHypergraph a
  -> Matching a -> Wire a Open -> f (Wire a Open)
candidates pattern context m w
  = case (determined pattern context m w) of
      Just w  -> pure w
      Nothing -> undetermined pattern context m w

-- | Return the context wire uniquely determined by a pattern wire.
-- If no context wire is so determined, return Nothing.
determined
  :: Ord sig
  => OpenHypergraph sig
  -> OpenHypergraph sig
  -> Matching sig
  -> Wire sig Open
  -> Maybe (Wire sig Open)
determined pattern context m w@(s, t) =
  case (counterpart s, counterpart t) of
    (Nothing, Nothing) -> Nothing
    (Just s', Nothing) -> (s',) <$> target s' context
    (Nothing, Just t') -> (,t') <$> source t' context
    (Just s', Just t') -> Just (s', t')

  where
    -- Given a port in the pattern, find its counterpart in the context by
    -- simply looking up the hyperedge ID.
    -- counterpart :: Port sig a Open -> Maybe (Port sig a Open)
    counterpart (Port Boundary _) = Nothing
    counterpart (Port (Gen (_, e)) i)  = do
      e' <- Bimap.lookup e (_matchingEdges m)
      t' <- Map.lookup e' (signatures context)
      return (Port (Gen (t', e')) i)

-- If a wire is not uniquely determined by the current matching,
-- this function will search for an appropriate unmatched wire
-- TODO: use an index here. We can index by the "portsMatch" condition- see
-- wiki.
undetermined
  :: (Eq a, Ord a, MonadLogic f)
  => OpenHypergraph a
  -> OpenHypergraph a
  -> Matching a
  -> Wire a Open
  -> f (Wire a Open)
undetermined pattern context m w = do
  choice $ filter condition (Bimap.toList $ connections context)
  where condition = consistent pattern context m w

-- | Given a proposed wire matching, check for consistency with the rest of the
-- matching.
consistent
  :: (Eq a, Ord a)
  => OpenHypergraph a
  -> OpenHypergraph a
  -> Matching a
  -> Wire a Open
  -> Wire a Open
  -> Bool
consistent pattern context m a b
  =  portsMatch a b
  && unmatchedContextWire m b
  && edgeTypesMatch pattern context a b -- this is expensive so important to come last

-- | Returns true if a (context) wire is not in the Matching
unmatchedContextWire :: Ord a => Matching a -> Wire a Open -> Bool
unmatchedContextWire m contextWire =
  not $ Bimap.memberR contextWire (_matchingWires m)

portsMatch :: Wire a Open -> Wire a Open -> Bool
portsMatch (s, t) (s', t') = portMatch s s' && portMatch t t'

-- A boundary port in the pattern can match anything in the context;
-- a generator in the pattern can never match a boundary in the context;
-- two generators ports can only  match if their indices are equal.
portMatch :: Port sig a Open -> Port sig a Open -> Bool
portMatch (Port Boundary _) _   = True
portMatch _ (Port Boundary _)   = False
portMatch (Port _ i) (Port _ j) = i == j

edgeTypesMatch
  :: Eq a
  => OpenHypergraph a -> OpenHypergraph a -> Wire a Open -> Wire a Open -> Bool
edgeTypesMatch pattern context (p, q) (p', q')
  =  edgeTypeMatch pattern context p p'
  && edgeTypeMatch pattern context q q'

-- TODO: NOTE: this is a bit redundant with portMatch above!
edgeTypeMatch
  :: Eq a
  => OpenHypergraph a
  -> OpenHypergraph a
  -> Port a x Open -> Port a x Open -> Bool
edgeTypeMatch pattern context (Port p _) (Port p' _) =
  case (p, p') of
    (Boundary, _)             -> True
    (_, Boundary)             -> False
    (Gen (t1,_), Gen (t2,_))  -> t1 == t2
