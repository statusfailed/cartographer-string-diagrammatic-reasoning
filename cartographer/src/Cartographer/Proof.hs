{-# LANGUAGE TupleSections #-}
module Cartographer.Proof
  ( Theory(..)
  , Proof(..)
  , Rule
  , rule
  , ProofStep(..)
  , proof
  , matchingRules
  , applyRule
  , undo
  ) where

import Data.Set (Set)

import Data.Hypergraph
import Cartographer.Layout

data Rule sig = Rule
  { _ruleLhs :: Layout sig
  , _ruleRhs :: Layout sig
  } deriving(Eq, Ord, Show)

rule :: Signature sig => Layout sig -> Layout sig -> Maybe (Rule sig)
rule l r
  | isComplete hgL && isComplete hgR && toSize hgL == toSize hgR =
      Just $ Rule l r

  | otherwise =
      Nothing
  where
    hgL = hypergraph l
    hgR = hypergraph r

data Theory sig = Theory
  { _theoryGenerators :: Set sig
  , _theoryAxioms     :: [Rule sig]
  } deriving(Eq, Ord, Show)

-- | Proofs begin with an initial term, and have a sequence of 'ProofStep's
-- transforming it.
data Proof sig = Proof
  { _proofTerm  :: Layout sig -- ^ current state / term of the proof
  , _proofSteps :: [ProofStep sig] -- ^ steps taken to get here
  } deriving(Eq, Ord, Show)

data ProofStep sig = ProofStep
  { _proofStepTerm     :: Layout sig     -- ^ The current proof state (i.e. term)
  , _proofStepRule     :: Rule sig       -- ^ The rewrite rule applied
  , _proofStepLhsMatch :: MatchState sig -- ^ a matching of the LHS in the term
  } deriving(Eq, Ord, Show)

-- | create a new proof from a complete Layout
proof :: Signature sig => Layout sig -> Maybe (Proof sig)
proof term = case isComplete (hypergraph term) of
  True  -> Just (Proof term [])
  False -> Nothing

-- | Which rules of a theory can be applied to the current proof state?
-- NOTE: some rules will appear more than once if they have multiple valid matchings.
matchingRules :: Eq sig => Theory sig -> Layout sig -> [(Rule sig, MatchState sig)]
matchingRules (Theory _ axioms) term = axioms >>= f
  where f rule@(Rule lhs _) = fmap (rule,) (matchLayout lhs term)

-- | Apply a rule by a specific matching in the current term of the proof.
applyRule
  :: Generator sig
  => (Rule sig, MatchState sig) -> Proof sig -> Proof sig
applyRule (rule@(Rule lhs rhs), lhsMatch) (Proof term steps) =
  Proof term' (step:steps)
  where
    (term', _) = rewriteLayout lhsMatch rhs term
    step       = ProofStep term rule lhsMatch

-- | Pop a step off the stack, and undo the last rewrite.
undo :: Proof sig -> Proof sig
undo proof@(Proof _ []) = proof -- can't pop an empty stack, son
undo (Proof _ (step:steps)) = Proof (_proofStepTerm step) steps
