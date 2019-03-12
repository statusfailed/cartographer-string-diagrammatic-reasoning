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
import Control.Monad

import Data.Functor.Identity
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.State
import Control.Monad.State.Class

import Control.Monad.Logic

-- | The read-only data available during matching - the pattern and graph to match it in.
data MatchEnv sig = MatchEnv
  { _matchEnvPattern :: OpenHypergraph sig
  , _matchEnvGraph   :: OpenHypergraph sig
  } deriving (Eq, Ord, Read, Show)

-- | A matching consists of two 1:1 correspondences:
--    1) Between ports in the pattern and the graph
--    2) Between hyperedge IDs in the pattern and in the graph
data MatchState sig = MatchState
  { _matchStatePortsL :: Bimap (Port L Open) (Port L Open)
  , _matchStatePortsR :: Bimap (Port R Open) (Port R Open)
  -- ^ A 1:1 correspondence of ports in the pattern (left) and the graph (right)
  , _matchStateEdges :: Bimap HyperEdgeId HyperEdgeId
  -- ^ A 1:1 correspondence between hyperedges in the pattern (left) and graph (right)
  }

deriving instance Eq (MatchState sig)
deriving instance Ord (MatchState sig)
deriving instance Show (MatchState sig)

-- TODO: add MonadLogic :-)
class
  ( MonadReader (MatchEnv sig) m
  , MonadState  (MatchState sig) m
  , MonadLogic m
  ) => MonadMatch sig m

type MatchT sig m = ReaderT (MatchEnv sig) (StateT (MatchState sig) (LogicT m))
type Match sig = MatchT sig Identity

instance Monad m => MonadMatch sig (MatchT sig m)

emptyMatchState :: MatchState sig
emptyMatchState = MatchState Bimap.empty Bimap.empty Bimap.empty

runMatchT
  :: Monad m
  => OpenHypergraph sig
  -> OpenHypergraph sig
  -> MatchT sig m a
  -> m [(a, MatchState sig)]
runMatchT pattern graph =
  observeAllT . flip runStateT emptyMatchState . flip runReaderT env
  where env = MatchEnv pattern graph

runMatch
  :: OpenHypergraph sig
  -> OpenHypergraph sig
  -> Match sig a
  -> [(a, MatchState sig)]
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
    -- TODO: remove evil partial functions.
    -- This indicates a programmer error- only caused by a malformed Hypergraph
    Nothing -> error $ "Data.Hypergraph.Match.sigOf: no signature for " ++ show e

matchHyperEdgeId
  :: (Eq sig, MonadMatch sig m)
  => HyperEdgeId
  -> HyperEdgeId
  -> m HyperEdgeId
matchHyperEdgeId i j = do
  -- signatures of each hyperedge must be equal
  liftM2 (==) (sigOf _matchEnvPattern i) (sigOf _matchEnvGraph j) >>= guard

  -- either i matches j, or neither are matched
  gets (pairedOrMissing i j . _matchStateEdges) >>= guard

  -- set i == j
  modify (\m -> m { _matchStateEdges = Bimap.insert i j (_matchStateEdges m) })
  return j

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
matchPort p@(Port (Gen pe) pi) q@(Port (Gen qe) qi) = void $ do
  -- Ports must connect at the same place on the hyperedge
  guard $ pi == qi
  -- NOTE: doesn't set p <-> q, that's done in matchWire.
  matchHyperEdgeId pe qe

-- | Propose that a given "wire" in the pattern matches a wire in the graph.
-- TODO: remove the "when" blocks- use a lens to parametrise "matchPort" over
-- the matched Source/Target ports. The code below is hideous!!
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
  -- TODO: check both matched
  when (not $ isBoundary pa) $
    gets (pairedOrMissing pa qa . _matchStatePortsL) >>= guard
  modify
    (\m -> m { _matchStatePortsL = Bimap.insert pa qa (_matchStatePortsL m) })

  matchPort pb qb
  when (not $ isBoundary pb) $
    gets (pairedOrMissing pb qb . _matchStatePortsR) >>= guard
  modify
    (\m -> m { _matchStatePortsR = Bimap.insert pb qb (_matchStatePortsR m) })
  where
    isBoundary (Port Boundary _) = True
    isBoundary _ = False


-------------------------------
-- Matching

-- | A list of unmatched edges. That is, edges present in the context, but
-- not (yet) in the matching
unmatchedEdges :: MonadMatch sig m => m [HyperEdgeId]
unmatchedEdges = do
  graphEdges <- asks (Map.keys . signatures . _matchEnvGraph)
  mat <- get
  let f e = Bimap.memberR e (_matchStateEdges mat)
  return (filter (not . f) graphEdges)

-- | A list of unmatched *source* ports.
-- That is, source ports present in the context, but not in the matching.
unmatchedPorts :: MonadMatch sig m => m [Port L Open]
unmatchedPorts = do
  graphPorts <- asks (Map.keys . connections . _matchEnvGraph)
  mat <- get
  let f q = Bimap.memberR q (_matchStatePortsL mat)
  return (filter (not . f) graphPorts)

-- | Return a list of matching candidates for a port in the pattern.
-- Specifically, for a given port in the PATTERN, this returns a LIST of ports
-- in the GRAPH.
--
-- If the HyperEdgeId of that port is already matched, this will return exactly
-- one candidate port in the graph- because it's uniquely determined by its
-- position on the hyperedge boundary.
--
-- Otherwise, it returns a list of possible matches.
matchCandidates
  :: (Eq sig, MonadMatch sig m) => Port L Open -> m [Port L Open]
-- *any* port in an *unmatched* hyperedge?
-- TODO: maybe the hyperedge shouldn't be matched?
matchCandidates (Port Boundary i) = unmatchedPorts
matchCandidates (Port (Gen e) i) = do
  matchedEdges <- _matchStateEdges <$> get
  case Bimap.lookup e matchedEdges of
    -- this port must exist, if the signatures of e and e' match, which they
    -- do if they were matched.
    Just e' -> return [Port (Gen e') i]
    -- Any unmatched ith port in an unmatched hyperedge
    -- TODO: this is quadraticish when it should be linearish- maybe keep the
    -- list of unmatched ports and edges up-to-date?
    -- TODO: actually check if a the ith port of each generator is matched!
    Nothing -> fmap (\e' -> Port (Gen e') i) <$> unmatchedEdges

-- | A single step of matching: given a single port in the pattern,
-- find the wire it is part of, and match it to one in the graph.
-- If the port is already matched, this does nothing.
matchStep :: (Eq sig, MonadMatch sig m) => Port L Open -> m ()
matchStep ps = do
  pt <- toTarget ps . _matchEnvPattern <$> ask

  -- Propose all possible match candidates for the corresponding graph port.
  qs <- msum . fmap return =<< matchCandidates ps
  qt <- toTarget qs . _matchEnvGraph <$> ask

  matchWire (ps, pt) (qs, qt)

{-# WARNING toTarget "partial function" #-}
-- | TODO: REMOVE PARTIAL FUNCTION
-- Idea: replace with "fail all matches", or allow disconnected generators to
-- match?
toTarget :: Port L Open -> OpenHypergraph sig -> Port R Open
toTarget src = maybe onErr id . Map.lookup src . connections
  where
    onErr = error $ "cannot match unconnected port " ++ show src

matchAll
  :: (Eq sig, MonadMatch sig m)
  => [Port L Open]
  -> m ()
matchAll = void . mapM matchStep

-- | True if all pattern ports have been matched.
allMatched :: MonadMatch sig m => m Bool
allMatched = do
  numMatchedWires <- Bimap.size . _matchStatePortsL <$> get
  numPatternWires <- Map.size . connections . _matchEnvPattern <$> ask
  return $ numPatternWires == numMatchedWires

-- | Match a pattern in a context.
-- Pseudocode:
--    * Get a list of all source ports by breadth-first traversal of the
--    pattern (or any order, but BFS for efficiency!)
--    * For each source port:
--      * Try to match each wire (uniquely defined by a source port)
match :: Eq sig => OpenHypergraph sig -> OpenHypergraph sig -> [MatchState sig]
match pattern graph = fmap snd $ runMatch pattern graph (matchAll ps)
  where
    ps = Map.keys (connections pattern)
    -- TODO: for efficiency, use bfs.
    -- Left ports, matched in breadth-first order.
    ps' = Hypergraph.bfs pattern
