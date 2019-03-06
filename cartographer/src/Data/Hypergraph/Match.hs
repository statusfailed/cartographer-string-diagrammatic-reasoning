{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Pattern matching hypergraphs
module Data.Hypergraph.Match where

import Data.Hypergraph.Type as Hypergraph

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

import Control.Applicative
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.State.Class

-- | The read-only data available during matching - the pattern and graph to match it in.
data MatchEnv sig = MatchEnv
  { _matchEnvPattern :: OpenHypergraph sig
  , _matchEnvGraph   :: OpenHypergraph sig
  } deriving (Eq, Ord, Read, Show)

-- | A matching consists of two 1:1 correspondences:
--    1) Between ports in the pattern and the graph
--    2) Between hyperedge IDs in the pattern and in the graph
data Match sig = Match
  { _matchPorts :: Bimap (Port L Open) (Port R Open)
  -- ^ A 1:1 correspondence of ports in the pattern (left) and the graph (right)
  , _matchEdges :: Bimap HyperEdgeId HyperEdgeId
  -- ^ A 1:1 correspondence between hyperedges in the pattern (left) and graph (right)
  }

-- TODO: add MonadLogic :-)
class (MonadReader (MatchEnv sig) m, MonadState (Match sig) m, Alternative m) => MonadMatch sig m

emptyMatch :: Match sig
emptyMatch = Match Bimap.empty Bimap.empty

-- | Returns True if either:
--    a and b are a pair in the map
--    neither a nor b are in the map
-- That is, returns False when
--    a is matched to b' != b, or
--    b is matched to a' != a
-- TODO: check this is right- what about case (Just b', Nothing) ?
pairedOrMissing :: (Eq a, Ord a, Eq b, Ord b) => a -> b -> Bimap a b -> Bool
pairedOrMissing a b m = case (Bimap.lookup a m, Bimap.lookupR b m) of
  (Just b', Just a') -> b' == b && a' == a
  (Nothing, Nothing) -> True
  _ -> False

sigOf
  :: MonadReader (MatchEnv sig) m
  => (MatchEnv sig -> OpenHypergraph sig) -> HyperEdgeId -> m sig
sigOf f e = do
  sig <- reader (Map.lookup e . Hypergraph.signatures . f)
  case sig of
    Just x  -> return x
    Nothing -> error $ "Data.Hypergraph.Match.sigOf: no signature for " ++ show e

matchHyperEdgeId
  :: (Eq sig, MonadMatch sig m)
  => HyperEdgeId
  -> HyperEdgeId
  -> m HyperEdgeId
matchHyperEdgeId p q = do
  -- signatures of each hyperedge must be equal
  liftM2 (==) (sigOf _matchEnvPattern p) (sigOf _matchEnvGraph q) >>= guard

  -- either p matches q, or neither are matched
  gets (pairedOrMissing p q . _matchEdges) >>= guard

  return q

-- | Match two ports
-- (i.e., assert that pattern port p corresponds to graph port q.)
matchPort
  :: (Eq sig, MonadMatch sig m)
  => Port side Open
  -> Port side Open
  -> m ()
matchPort (Port Boundary pi) q = do
  return () -- match success, TODO state update
matchPort (Port (Gen pe) pi) (Port Boundary qi) = return () -- impossible!
matchPort (Port (Gen pe) pi) (Port (Gen qe) qi) = do
  -- Ports must connect at the same place on the hyperedge
  guard $ pi == qi
  -- TODO: handle Left and Right boundaries!
  matchHyperEdgeId pe qe
  return ()

-- | Propose that a given "wire" in the pattern matches a wire in the graph.
matchWire
  :: (Eq sig, MonadMatch sig m)
  => (Port L Open, Port R Open)
  -- ^ A wire in the pattern is uniquely identified by the two ports it connects
  -> (Port L Open, Port R Open)
  -- ^ An proposed matching wire in the graph
  -> m ()
matchWire (pa, pb) (qa, qb) = do
  -- If pa is matches to qa, then the hyperedges they are part of must also match:
  -- pa <-> qa => e(pa) = e(qa)
  -- If pa connects to pb, and qa connects to qb, then pb must match with qb.
  -- pa ~> pb AND qa ~> qb => pb <-> qb
  -- pb <-> qb => e(pb) = e(qb)
  matchPort pa qa
  matchPort pb qb

-- | Match a pattern within a context.
-- Pseudocode:
--  1. Select a pattern port not yet matched (the shallowest? deepest?)
--  2. propose a match in the context
--  3. if the match violates a constraint, abort
--
-- NOTE: this is a nondeterministic algorithm, so really we're trying all
--       branches.
--
-- Rules for "valid match"
--  * a Left or Right will match any other port (modulo hyperedge constraints!)
match
  :: Eq sig
  => MonadMatch sig m
  => m (Match sig)
match = do
  error "TODO: match"

-- Probably the exposed interface for matching?
match' :: OpenHypergraph sig -> OpenHypergraph sig -> [Match sig]
match' = undefined
