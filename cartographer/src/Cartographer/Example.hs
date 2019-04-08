module Cartographer.Example where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

import Data.Hypergraph hiding (identity)
import Cartographer.Layout as Layout
import qualified Cartographer.Types.Grid as Grid
import qualified Data.Equivalence as Equivalence
import Linear.V2 (V2(..))

import Data.Hypergraph.Examples

instance Generator NonConvexGen where
  generatorHeight E1 = 1
  generatorHeight _  = 3

  generatorInputs E1 = [0]
  generatorInputs E2 = [1]
  generatorInputs E3 = [0,2]

  generatorOutputs E1 = [0]
  generatorOutputs E2 = [0,2]
  generatorOutputs E3 = [1]

data Gen = Copy | Discard
  deriving(Eq, Ord, Read, Show)

instance Signature Gen where
  toSize Copy     = (1,2)
  toSize Discard  = (1,0)

instance Generator Gen where
  generatorHeight Copy    = 3
  generatorHeight Discard = 1

  generatorInputs Copy    = [1]
  generatorInputs Discard = [0]

  generatorOutputs Copy = [0,2]
  generatorOutputs Discard = []

copyDiscard
  = id
  . connectPorts (Port Boundary 0) (Port (Gen 0) 0)
  . connectPorts (Port (Gen 0) 1) (Port Boundary 0)
  . connectPorts (Port (Gen 0) 0) (Port (Gen 1) 0)
  . snd . placeGenerator Discard (V2 1 0)
  . snd . placeGenerator Copy (V2 0 0)
  $ Layout.empty

identity = connectPorts (Port Boundary 0) (Port Boundary 0) Layout.empty

-------------------------------
-- bug hunting

  -- match         lhs context
  -- rewrite match rhs context
{-(m : _) = match (hypergraph copyDiscard) (hypergraph copyDiscard)-}
{-(r , _) = rewriteLayout m identity copyDiscard-}
r = identity { hypergraph = (hypergraph identity) { Data.Hypergraph.nextHyperEdgeId = 2 } }
(m': _) = match (hypergraph identity) (hypergraph r)
(r', rm') = rewriteLayout m' copyDiscard r

-------------------------------
-- debug

printMatching m = do
  putStrLn "source"
  mapM_ print $ Bimap.toList $ _matchStatePortsSource rm'

  putStrLn "\ntarget"
  mapM_ print $ Bimap.toList $ _matchStatePortsTarget rm'

  putStrLn "\nedges"
  mapM_ print $ Bimap.toList $ _matchStateEdges rm'


prettyGrid (Grid.Grid (Equivalence.Equivalence cls ms)) =
  uncurry f =<< Map.toList ms
  where
    f (TileHyperEdge  (HyperEdgeId e)) vs = show e ++ "  ->  " ++ show vs ++ "\n"
    f (TilePseudoNode _)               vs = "P  ->  " ++ show vs ++ "\n"

prettyLayout :: Show sig => Layout sig -> String
prettyLayout (Layout hg g)
  =  prettyPrint hg
  ++ "grid\n-------------------------------\n"
  ++ prettyGrid g

printLayout :: Show sig => Layout sig -> IO ()
printLayout = putStrLn . prettyLayout
