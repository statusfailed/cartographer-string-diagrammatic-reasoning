module Cartographer.Example where

import Data.Hypergraph
import Cartographer.Layout as Layout
import Linear.V2 (V2(..))

data Gen = Copy | Discard
  deriving(Eq, Ord, Read, Show)

instance Signature Gen where
  toSize Copy     = (1,2)
  toSize Discard  = (0,1)

instance Generator Gen where
  generatorHeight Copy    = 3
  generatorHeight Discard = 1

  generatorInputs Copy    = [1]
  generatorInputs Discard = [0]

  generatorOutputs Copy = [0,2]
  generatorOutputs Discard = []

lhs
  = id
  . connectPorts (Port Boundary 0) (Port (Gen 0) 0)
  . connectPorts (Port (Gen 0) 1) (Port Boundary 0)
  . connectPorts (Port (Gen 0) 0) (Port (Gen 1) 0)
  . snd . placeGenerator Discard (V2 1 0)
  . snd . placeGenerator Copy (V2 0 0)
  $ Layout.empty

rhs = connectPorts (Port Boundary 0) (Port Boundary 0) Layout.empty

context = lhs

[m] = match (hypergraph lhs) (hypergraph context)

rhg = rewrite m (hypergraph rhs) (hypergraph context)

r = rewriteLayout m rhs context

-------------------------------
-- debug

f :: Show sig => OpenHypergraph sig -> IO ()
f = putStrLn . Data.Hypergraph.prettyPrint

g :: Show sig => Layout sig -> IO ()
g = f . hypergraph
