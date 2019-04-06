-- Design
--
-- "*Builder" modules represent editors where the "editing" state might not
-- represent a finished product - e.g., Rules might be invalid until the left
-- diagram has the same number of inputs and outputs as the right.
module Cartographer.UI where

import qualified Cartographer.Components.Collapsible as Collapsible
import qualified Cartographer.Components.Sequence as Sequence

import qualified Cartographer.Components.TheoryEditor as TheoryEditor
import qualified Cartographer.Components.ProofWrapper as ProofWrapper

import qualified Cartographer.Components.RuleBuilder as RuleBuilder

data Model = Model
  { theoryEditor :: Collapsible.Model TheoryEditor.Model -- ^ a single theory editor
  , proofWrapper :: ProofWrapper.Model
  } deriving(Eq, Ord, Show)

data Action
  = TheoryEditorAction (Collapsible.Action TheoryEditor.Action)
  | ProofWrapperAction ProofWrapper.Action
  deriving(Eq, Ord, Show)
