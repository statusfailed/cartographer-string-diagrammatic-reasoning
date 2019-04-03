module Data.Hypergraph.Examples where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

import Data.Hypergraph.Type
import Data.Hypergraph.Match

-------------------------------
-- A simple example: two 1x1 generators, matching 
data SimpleGen = SimpleGen
  deriving(Eq, Ord, Read, Show)

sizeOfSimple SimpleGen = (1,1)

simplePattern :: OpenHypergraph SimpleGen
simplePattern = Hypergraph conns sigs 1 where
  sigs = Map.fromList [(0, SimpleGen)]
  conns = Bimap.fromList
    [ (Port Boundary 0, Port (Gen 0) 0)
    , (Port (Gen 0) 0, Port Boundary 0)
    ]

simpleGraph = Hypergraph conns sigs 2 where
  sigs = Map.fromList [(0, SimpleGen), (1, SimpleGen)]
  conns = Bimap.fromList
    [ (Port Boundary 0, Port (Gen 0) 0)
    , (Port (Gen 0)  0, Port (Gen 1) 0)
    , (Port (Gen 1)  0, Port Boundary 0)
    ]

-------------------------------
-- The paper's non-convex matching example

data NonConvexGen = One | Two
  deriving(Eq, Ord, Read, Show)
sizeOfNonConvex One = (1,1)
sizeOfNonConvex Two = (2,2)

nonConvexPattern = Hypergraph conns edges 2
  where
    edges = Map.fromList [(0, Two), (1, Two)]
    conns = Bimap.fromList $
      -- gen 0 in
      [ (Port Boundary 1, Port (Gen 0) 0)
      , (Port Boundary 2, Port (Gen 0) 1)
      -- gen 0 out
      , (Port (Gen 0)  0, Port Boundary 0)
      , (Port (Gen 0)  1, Port (Gen 1) 1)
      -- gen 1 in
      , (Port Boundary 0, Port (Gen 1) 0)
      -- gen 1 out
      , (Port (Gen 1)  0, Port Boundary 0)
      , (Port (Gen 1)  1, Port Boundary 1)
      ]

nonConvexGraph = Hypergraph conns edges
  where
    edges = Map.fromList [(0, Two), (1, Two), (2, One)]
    conns = Bimap.fromList $
      -- gen 0 in
      [ (Port Boundary 1, Port (Gen 0) 0)
      , (Port Boundary 2, Port (Gen 0) 1)
      -- gen 0 out
      , (Port (Gen 0)  0, Port (Gen 2) 0) -- NOTE DIFFERENCE
      -- gen 0 ~> gen 1
      , (Port (Gen 0)  1, Port (Gen 1) 1)
      -- gen 1 in
      , (Port (Gen 2)  0, Port (Gen 1) 0)
      -- gen 1 out
      , (Port (Gen 1)  0, Port Boundary 0)
      , (Port (Gen 1)  1, Port Boundary 1)
      ]
