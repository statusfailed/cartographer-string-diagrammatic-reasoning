{-# LANGUAGE OverloadedStrings #-}
-- | Preloaded theories for demo purposes in the Cartographer UI.
module Cartographer.Preload where

import Data.Maybe (catMaybes)

import Data.Hypergraph
import Cartographer.Viewer (Generator(..))

import Cartographer.Layout (Layout(..), layout)
import qualified Cartographer.Layout as Layout

import Cartographer.Proof

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

bialgebra :: Theory Generator
bialgebra = Theory (Set.fromList generators) axioms
  where
    generators :: [Generator]
    generators = [copy, discard, add, unit]

    axioms = catMaybes $
      [ rule (toLayout identity)      (toLayout copyDiscard0)
      , rule (toLayout copyDiscard0)  (toLayout identity)
      , rule (toLayout identity)      (toLayout copyDiscard1)
      , rule (toLayout copyDiscard1)  (toLayout identity)
      ]


copy    = Generator (1,2) ([1], [0,2]) "red" "copy"
discard = Generator (1,0) ([0], []) "red" "discard"
add     = Generator (2,1) ([0,2], [1]) "lime" "add"
unit    = Generator (0,1) ([], [0]) "lime" "unit"

-- TODO: should not have to call recomputePseudoNodes here... gross.
toLayout :: Layout.Generator sig => OpenHypergraph sig -> Layout sig
toLayout hg = Layout.recomputePseudoNodes $ Layout hg (layout hg)

copyDiscard0 = Hypergraph conns sigs 2
  where
    sigs  = Map.fromList [(0, copy), (1, discard)]
    conns = Bimap.fromList $
      [ (Port Boundary 0, Port (Gen 0) 0)
      , (Port (Gen 0) 0, Port (Gen 1) 0)
      , (Port (Gen 0) 1, Port Boundary 0)
      ]

-- same as copyDiscard0, but with the outputs of the copy twisted.
copyDiscard1 = Hypergraph conns sigs 2
  where
    sigs  = Map.fromList [(0, copy), (1, discard)]
    conns = Bimap.fromList $
      [ (Port Boundary 0, Port (Gen 0) 0)
      , (Port (Gen 0) 0, Port Boundary 0)
      , (Port (Gen 0) 1, Port (Gen 1) 0)
      ]
