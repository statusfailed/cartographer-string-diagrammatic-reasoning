{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Pattern matching hypergraphs
module Data.Hypergraph.Match where

import Data.Hypergraph.Type as Hypergraph

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Bimap (Bimap(..))
import qualified Data.Bimap as Bimap

import Control.Applicative
import Control.Monad hiding (guard)

import Data.Functor.Identity
import Control.Monad.Reader hiding (guard)
import Control.Monad.Reader.Class
import Control.Monad.State hiding (guard)
import Control.Monad.State.Class

guard x = return ()

-- | The read-only data available during matching - the pattern and graph to match it in.
data MatchEnv sig = MatchEnv
  { _matchEnvPattern :: OpenHypergraph sig
  , _matchEnvGraph   :: OpenHypergraph sig
  } deriving (Eq, Ord, Read, Show)

-- | A matching consists of two 1:1 correspondences:
--    1) Between ports in the pattern and the graph
--    2) Between hyperedge IDs in the pattern and in the graph
data MatchState sig = MatchState
  { _matchPortsL :: Bimap (Port L Open) (Port L Open)
  , _matchPortsR :: Bimap (Port R Open) (Port R Open)
  -- ^ A 1:1 correspondence of ports in the pattern (left) and the graph (right)
  , _matchEdges :: Bimap HyperEdgeId HyperEdgeId
  -- ^ A 1:1 correspondence between hyperedges in the pattern (left) and graph (right)
  }

deriving instance Eq (MatchState sig)
deriving instance Ord (MatchState sig)
deriving instance Show (MatchState sig)

-- TODO: add MonadLogic :-)
class
  ( MonadReader (MatchEnv sig) m
  , MonadState (MatchState sig) m
  ) => MonadMatch sig m

type MatchT sig m = ReaderT (MatchEnv sig) (StateT (MatchState sig) m)
type Match sig = MatchT sig Identity

instance Monad m => MonadMatch sig (MatchT sig m)

emptyMatchState :: MatchState sig
emptyMatchState = MatchState Bimap.empty Bimap.empty Bimap.empty

runMatchT
  :: Monad m
  => OpenHypergraph sig
  -> OpenHypergraph sig
  -> MatchT sig m a
  -> m (MatchState sig)
runMatchT pattern graph = flip execStateT emptyMatchState . flip runReaderT env
  where env = MatchEnv pattern graph

runMatch
  :: OpenHypergraph sig
  -> OpenHypergraph sig
  -> Match sig a
  -> MatchState sig
runMatch p g = runIdentity . runMatchT p g


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
    -- TODO: remove evil partial functions
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

  -- set p == q
  modify (\m -> m { _matchEdges = Bimap.insert p q (_matchEdges m) })
  return q

-- | Match two source ports or two target ports.
-- (i.e., assert that pattern port p corresponds to graph port q.)
matchPort
  :: (Eq sig, MonadMatch sig m)
  => Port side Open
  -> Port side Open
  -> m ()
-- match success, TODO state update
matchPort (Port Boundary pi) _ = return ()

-- A Generator node in the pattern can never match a Boundary in the graph-
-- the signatures clearly don't match!
matchPort _ (Port Boundary qi) = guard False

-- If both parts of the proposed match are Generators, then try to match the
-- edge of each port, and check each port has the same index on that edge.
matchPort p@(Port (Gen pe) pi) q@(Port (Gen qe) qi) = do
  -- Ports must connect at the same place on the hyperedge
  guard $ pi == qi
  -- set p <-> q

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
  modify (\m -> m { _matchPortsL = Bimap.insert pa qa (_matchPortsL m) })

  matchPort pb qb
  modify (\m -> m { _matchPortsR = Bimap.insert pb qb (_matchPortsR m) })

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
  => m (MatchState sig)
match = do
  error "TODO: match"

-- Probably the exposed interface for matching?
match' :: OpenHypergraph sig -> OpenHypergraph sig -> [MatchState sig]
match' = undefined


-------------------------------
-- Testing

proggy :: Eq sig => Match sig ()
proggy = do
  matchWire
    (Port Boundary 0, Port Boundary 0)
    (Port Boundary 0, Port Boundary 0)
  where
    pa = Port Boundary 0
    qa = Port Boundary 0
    pb = Port Boundary 0
    qb = Port Boundary 0

test f = runMatch identity identity $ f
