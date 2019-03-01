-- | Pattern matching hypergraphs
module Data.Hypergraph.Match where

import Data.Hypergraph.Type

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | A matching is a mappng of ports in one graph to another.
-- That is, an assignment of each port in the pattern to its corresponding port
-- in the matching context 
type Match sig = Map (Port Open) (Port Open)

-- | Match a pattern within a context.
-- Pseudocode:
--  1. Select the shallowest pattern port not yet matched
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
  => OpenHypergraph sig
  -- ^ Pattern to match
  -> OpenHypergraph sig
  -- ^ Context to find pattern in
  -> Match sig
match pattern graph = undefined

-- | Rewrite a pattern in a context, using a specific match.
rewrite
  :: OpenHypergraph sig
  -> Match sig
  -> OpenHypergraph sig
  -> OpenHypergraph sig
rewrite pattern match context = undefined
