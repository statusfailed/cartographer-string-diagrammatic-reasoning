module Data.Hypergraph
  ( module Data.Hypergraph.Type
  , module Data.Hypergraph.Algebraic
  , module Data.Hypergraph.Matching
  {-, module Data.Hypergraph.Match-}
  {-, module Data.Hypergraph.Rewrite-}
  {-, module Data.Hypergraph.Traversal-}
  {-, module Data.Hypergraph.Layer-}
  ) where

-- Hypergraph type and basic functions
import Data.Hypergraph.Type

-- Safe construction and combination of hypergraphs
import Data.Hypergraph.Algebraic

-- *Unsafe* construction and modification of hypergraphs.
-- import Data.Hypergraph.Unsafe

-- Hypergraph pattern matching
import Data.Hypergraph.Matching hiding (empty)

-- Extra traversals
import Data.Hypergraph.Search

-- Hypergraph pattern matching
{-import Data.Hypergraph.Match-}

-- Hypergraph rewriting
{-import Data.Hypergraph.Rewrite-}

-- Traversal
{-import Data.Hypergraph.Traversal-}

-- Layering
{-import Data.Hypergraph.Layer-}
