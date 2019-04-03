module Data.Hypergraph.Rewrite where

import Control.Monad

import Data.Hypergraph.Type   as Hypergraph
import Data.Hypergraph.Match  as Match

import Data.Bimap (Bimap(..))
import qualified Data.Bimap as Bimap

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
rewrite match rhs context = (c, s3)
  where
    a       = removeMatchEdges match context
    (b, s1) = addRhsEdges rhs context
    s2      = addBoundaries match s1
    (c, s3) = addRhsConnections rhs s2 b

-- | Remove all edges in a 'MatchState' from an 'OpenHypergraph'.
removeMatchEdges :: MatchState sig -> OpenHypergraph sig -> OpenHypergraph sig
removeMatchEdges matchState context = foldr deleteEdge context edges
  where
    -- list of the edges in the context which were part of the matching
    edges = fmap snd . Bimap.toList . _matchStateEdges $ matchState

-- | Add all the edges of a pattern into a context, and return the new graph,
-- and the correspondence between pattern edges and (new) context edges.
--
-- statefully with new matching
-- for each (id, generator) in rhs:
--    id' <- addEdge generator context
--    add (id <-> id')
addRhsEdges :: OpenHypergraph sig -> OpenHypergraph sig -> (OpenHypergraph sig, MatchState sig)
addRhsEdges rhs context = undefined
  {-where-}
    {-copyEdge id sig match g = addEdge sig g-}

-- | Add the boundary ports from a LHS matching to the RHS matching
addBoundaries :: MatchState sig -> MatchState sig -> MatchState sig
addBoundaries patternMatch replacementMatch = undefined
  -- for each source port, if it's a boundary, add the mapping to the replacementMatch
  -- same for targets

addRhsConnections
  :: OpenHypergraph sig
  -- ^ the rewrite rule\'s RHS
  -> MatchState sig
  -- ^ the current match state of the RHS, containing matched edges and boundary ports
  -> OpenHypergraph sig
  -- ^ the current context graph, containing the (new) RHS edges
  -> (OpenHypergraph sig, MatchState sig)
addRhsConnections rhs rhsMatching context = undefined
