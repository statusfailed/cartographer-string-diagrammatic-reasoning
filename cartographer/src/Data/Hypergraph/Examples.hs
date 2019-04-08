module Data.Hypergraph.Examples where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

import Data.Hypergraph.Type
import Data.Hypergraph.Match
import Data.Hypergraph.Rewrite

-------------------------------
-- A simple example: two 1x1 generators, matching
data SimpleGen = SimpleGen
  deriving(Eq, Ord, Read, Show)

instance Signature SimpleGen where
  toSize SimpleGen = (1,1)

simplePattern :: OpenHypergraph SimpleGen
simplePattern = Hypergraph conns sigs 1 where
  sigs = Map.fromList [(0, SimpleGen)]
  conns = Bimap.fromList
    [ (Port Boundary 0, Port (Gen 0) 0)
    , (Port (Gen 0) 0, Port Boundary 0)
    ]

simpleRhs = identity

simpleGraph = Hypergraph conns sigs 2 where
  sigs = Map.fromList [(0, SimpleGen), (1, SimpleGen)]
  conns = Bimap.fromList
    [ (Port Boundary 0, Port (Gen 0) 0)
    , (Port (Gen 0)  0, Port (Gen 1) 0)
    , (Port (Gen 1)  0, Port Boundary 0)
    ]

simpleRewrite =
  let [m1, m2] = match simplePattern simpleGraph
  in  rewrite m1 simpleRhs simpleGraph

-- if we apply a rewrite rule forwards then backwards, we should get back the
-- same graph (modulo IDs) - meaning the twice-applied rule graph should match
-- in the zero-applied rule graph exactly once.
invertRewrite =
  let (g, m') = simpleRewrite
      (g', _) = rewrite m' simplePattern g
  in  length (match g' simpleGraph) == 1

-------------------------------
-- The paper's non-convex matching example

data NonConvexGen = E1 | E2 | E3
  deriving(Eq, Ord, Read, Show)

sizeOfNonConvex E1 = (1,2)
sizeOfNonConvex E2 = (2,1)
sizeOfNonConvex E3 = (1,1)

instance Signature NonConvexGen where
  toSize = sizeOfNonConvex

-- ----\/-----
--     /\
-- -e1---e2---
nonConvexPattern = Hypergraph conns edges 2
  where
    edges = Map.fromList [(0, E1), (1, E2)]
    conns = Bimap.fromList $
      -- wires from left to right
      [ (Port Boundary 0, Port (Gen 1) 0)
      , (Port Boundary 1, Port (Gen 0) 0)
      , (Port (Gen 0)  0, Port Boundary 0)
      , (Port (Gen 0)  1, Port (Gen 1) 1)
      -- gen 1 out
      , (Port (Gen 1)  0, Port Boundary 1)
      ]

--     e3
--    / \
-- -e1---e2---
nonConvexGraph = Hypergraph conns edges 3
  where
    edges = Map.fromList [(0, E1), (1, E2), (2, E3)]
    conns = Bimap.fromList $
      -- gen 0 in
      [ (Port Boundary 0, Port (Gen 0) 0)
      , (Port (Gen 0)  0, Port (Gen 2) 0)
      , (Port (Gen 0)  1, Port (Gen 1) 1)
      , (Port (Gen 2)  0, Port (Gen 1) 0)
      , (Port (Gen 1)  0, Port Boundary 0)
      ]
