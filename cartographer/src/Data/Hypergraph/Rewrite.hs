module Data.Hypergraph.Rewrite where

import Control.Monad

import Data.Hypergraph.Type   as Hypergraph
import Data.Hypergraph.Match  as Match

import qualified Data.List as List

import Data.Bimap (Bimap(..))
import qualified Data.Bimap as Bimap

import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map

-- | Given a matching, a rewrite RHS for that matching, and a context for the
-- match, rewrite the context graph
--
-- 1) Remove any edges in context that are part of the matching
--  - this also removes any wires with those edges
-- 2) Add in the /edges/ of the RHS, and keep track of the correspondence
--    "RHS ID <-> Context ID" in a MatchState
-- 3) Add every boundary port in the LHS MatchState, to the RHS MatchState, i.e.
--    - if LHS has L_j -> e_i, then RHS has L_0 -> e_i
--    - if LHS has e_i -> R_j, then RHS has e_i -> R_j
-- 4) Add in all the non-boundary ports of RHS, by replacing generator IDs with
--    those in their match states
rewrite
  :: MatchState sig     -- ^ Matched pattern
  -> OpenHypergraph sig -- ^ Replacement for matched pattern (i.e. rule RHS)
  -> OpenHypergraph sig -- ^ Context of matching
  -> (OpenHypergraph sig, MatchState sig)
  -- ^ Rewritten graph and "match" subgraph identifying where RHS was added
rewrite match rhs context = (c, rhsMatch3)
  where
    a              = removeMatchEdges match context
    (b, rhsMatch1) = addRhsEdges rhs context
    rhsMatch2      = addBoundaries match rhsMatch1
    (c, rhsMatch3) = addRhsConnections rhs rhsMatch2 b

-- | Remove all edges in a 'MatchState' from an 'OpenHypergraph'.
removeMatchEdges :: MatchState sig -> OpenHypergraph sig -> OpenHypergraph sig
removeMatchEdges matchState context = foldr deleteEdge context edges
  where
    -- list of the edges in the context which were part of the matching
    edges = fmap snd . Bimap.toList . _matchStateEdges $ matchState

-------------------------------
-- Add RHS edges to the context

-- | Add all the edges of a pattern into a context, and return the new graph,
-- and the correspondence between pattern edges and (new) context edges.
--
-- statefully with new matching
-- for each (id, generator) in rhs:
--    id' <- addEdge generator context
--    add (id <-> id')
addRhsEdges
  :: OpenHypergraph sig
  -> OpenHypergraph sig
  -> (OpenHypergraph sig, MatchState sig)
addRhsEdges rhs context =
  let (context', pairs) = copyEdges rhs context
  in  (context', emptyMatchState { _matchStateEdges = Bimap.fromList pairs })

-- | Copy all the edges from the source hypegraph to the target hypergraph,
-- returning also the correspondence between source hyperedge IDs, and their
-- new IDs in the target.
copyEdges
  :: Hypergraph f sig
  -> Hypergraph f sig
  -> (Hypergraph f sig, [(HyperEdgeId, HyperEdgeId)])
copyEdges source target =
  List.mapAccumL f target $ Map.toList (signatures source)
  where
    f graph (e, sig) = let (e', graph') = addEdge sig graph in (graph', (e, e'))

-------------------------------
-- addBoundaries

-- | Add the boundary ports from a LHS matching to the RHS matching
--
-- NOTE: WARNING: this actually wipes out existing ports in the replacement
-- match- this only works because its just a step in 'rewrite', but it would be
-- totally broken to use by itself!
addBoundaries :: MatchState sig -> MatchState sig -> MatchState sig
addBoundaries lhsMatch replacementMatch = replacementMatch
  { _matchStatePortsSource =
      Bimap.filter (\x _ -> isBoundary x) . _matchStatePortsSource $ lhsMatch
  , _matchStatePortsTarget =
      Bimap.filter (\x _ -> isBoundary x) . _matchStatePortsTarget $ lhsMatch
  }

isBoundary :: Port a Open -> Bool
isBoundary (Port e _) = open True (const False) e

-------------------------------
-- addRhsConnections
-- translate the connections from the RHS graph into connections to add to the
-- context graph.

addRhsConnections
  :: OpenHypergraph sig
  -- ^ the rewrite rule\'s RHS
  -> MatchState sig
  -- ^ the current match state of the RHS, containing matched edges and
  -- boundary ports
  -> OpenHypergraph sig
  -- ^ the current context graph, containing the (new) RHS edges
  -> (OpenHypergraph sig, MatchState sig)
addRhsConnections rhs rhsMatching context = undefined
  where
    conns :: Bimap (Port Source Open) (Port Target Open)
    conns = connections rhs
