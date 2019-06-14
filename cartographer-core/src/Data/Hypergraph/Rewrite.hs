module Data.Hypergraph.Rewrite where

import Control.Monad

import Data.Hypergraph.Type   as Hypergraph
import Data.Hypergraph.Unsafe as Hypergraph
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
--    "RHS ID <-> Context ID" in a MatchState.
--    (Also add in the ports of each edge)
-- 3) Add every boundary port in the LHS MatchState, to the RHS MatchState, i.e.
--    - if LHS has L_j -> e_i, then RHS has L_0 -> e_i
--    - if LHS has e_i -> R_j, then RHS has e_i -> R_j
-- 4) Use the matching to add wires from the RHS into the Context
rewrite
  :: Signature sig
  => MatchState sig     -- ^ Matched pattern
  -> OpenHypergraph sig -- ^ Replacement for matched pattern (i.e. rule RHS)
  -> OpenHypergraph sig -- ^ Context of matching
  -> (OpenHypergraph sig, MatchState sig)
  -- ^ Rewritten graph and "match" subgraph identifying where RHS was added
rewrite lhsMatch rhs context = (c, rhsMatch2)
  where
    a              = removeLhsEdges lhsMatch context
    (b, rhsMatch1) = addRhsEdges rhs a
    rhsMatch2      = addBoundaries lhsMatch rhsMatch1
    c              = addRhsConnections rhs rhsMatch2 b

-- | Remove all edges in a 'MatchState' from an 'OpenHypergraph'.
removeLhsEdges :: MatchState sig -> OpenHypergraph sig -> OpenHypergraph sig
removeLhsEdges matchState context = foldr deleteEdge context edges
  where
    -- list of the edges in the context which were part of the matching
    edges = fmap snd . Bimap.toList . _matchStateEdges $ matchState

-------------------------------
-- addRhsEdges

-- 1) Copy all edges + non-boundary ports into the matching, and add to the
-- context graph.
-- 2) Copy all RHS boundary ports into the matching
-- 3) Add connections to context graph
addRhsEdges
  :: Signature sig
  => OpenHypergraph sig
  -> OpenHypergraph sig
  -> (OpenHypergraph sig, MatchState sig)
addRhsEdges rhs context =
  foldr f (context, emptyMatchState) (Map.toList $ signatures rhs)
  where
    f (e, sig) (g, m) = copyEdge e sig g m

copyEdge
  :: Signature sig
  => HyperEdgeId
  -- ^ ID of edge in RHS pattern
  -> sig
  -- ^ signature of edge
  -> OpenHypergraph sig
  -- ^ Context graph
  -> MatchState sig
  -- ^ Current match state to modify
  -> (OpenHypergraph sig, MatchState sig)
  -- ^ Updated context graph and new match state
copyEdge e sig graph match = (graph', match')
  where
    (e', graph') = addEdge sig graph
    match' = match
      { _matchStatePortsSource =
          insertAll (_matchStatePortsSource match) sourcePorts

      , _matchStatePortsTarget =
          insertAll (_matchStatePortsTarget match) targetPorts

      , _matchStateEdges =
          Bimap.insert e e' (_matchStateEdges match)
      }

    (n, k) = toSize sig
    sourcePorts = f e e' k
    targetPorts = f e e' n
    f e e' m = [ (Port (Gen e) i, Port (Gen e') i) | i <- [0..m-1] ]

-------------------------------
-- addBoundaries

-- | Copy the boundary ports from a LHS matching to the RHS matching
addBoundaries :: MatchState sig -> MatchState sig -> MatchState sig
addBoundaries lhsMatch replacementMatch = replacementMatch
  { _matchStatePortsSource =
      insertAll (_matchStatePortsSource replacementMatch) sources
  , _matchStatePortsTarget =
      insertAll (_matchStatePortsTarget replacementMatch) targets
  }
  where
    sources = boundaryPorts (_matchStatePortsSource lhsMatch)
    targets = boundaryPorts (_matchStatePortsTarget lhsMatch)

-------------------------------
-- addRhsConnections
-- translate the connections from the RHS graph into connections to add to the
-- context graph.

addRhsConnections
  :: OpenHypergraph sig
  -- ^ the rewrite rule\'s RHS
  -> MatchState sig
  -- ^ the current match state of the RHS, containing matched edges and ports
  -> OpenHypergraph sig
  -- ^ the current context graph, containing the (new) RHS edges
  -> OpenHypergraph sig
addRhsConnections rhs rhsMatching context =
  foldr (maybe id $ uncurry connect) context wires
  where
    wires = fmap f . Bimap.toList . connections $ rhs
    f = uncurry (translateWire rhsMatching)

-- | Look up a source and target port in a matching and return the matched
-- versions.
translateWire
  :: MatchState sig
  -> Port Source Open
  -> Port Target Open
  -> Maybe (Port Source Open, Port Target Open)
translateWire m s t = liftM2 (,) s' t'
  where
    s' = Bimap.lookup s (_matchStatePortsSource m)
    t' = Bimap.lookup t (_matchStatePortsTarget m)

-------------------------------
-- Utilities

insertAll :: (Ord a, Ord b) => Bimap a b -> [(a, b)] -> Bimap a b
insertAll = foldr (uncurry Bimap.insert)

-- TODO: rename this- it means "ports which are boundaries in the pattern but
-- not necessarily the context"
boundaryPorts :: Bimap (Port a Open) (Port a Open) -> [(Port a Open, Port a Open)]
boundaryPorts = Bimap.toList . Bimap.filter (\x _ -> isBoundary x)
