-- Design
--
-- "*Builder" modules represent editors where the "editing" state might not
-- represent a finished product - e.g., Rules might be invalid until the left
-- diagram has the same number of inputs and outputs as the right.
module Cartographer.UI where

import Miso

import qualified Cartographer.Components.Collapsible as Collapsible
import qualified Cartographer.Components.Sequence as Sequence

import qualified Cartographer.Components.TheoryEditor as TheoryEditor
import qualified Cartographer.Components.ProofWrapper as ProofWrapper

import qualified Cartographer.Components.RuleBuilder as RuleBuilder

data Model = Model
  { theoryEditor :: Collapsible.Model TheoryEditor.Model -- ^ a single theory editor
  , proofWrapper :: ProofWrapper.Model
  } deriving(Eq, Ord, Show)

emptyModel :: Model
emptyModel = Model
  (Collapsible.newModel TheoryEditor.emptyModel)
  ProofWrapper.emptyModel

data Action
  = TheoryEditorAction (Collapsible.Action TheoryEditor.Action)
  | ProofWrapperAction ProofWrapper.Action
  deriving(Eq, Ord, Show)

update :: Action -> Model -> Model
update a (Model te pw) = case a of
  TheoryEditorAction a ->
    Model (Collapsible.update TheoryEditor.update a te) pw
  ProofWrapperAction a -> Model te pw -- TODO!

-- TODO: ProofWrapper
view :: Model -> View Action
view (Model te _) =
  TheoryEditorAction <$> Collapsible.view viewFull viewSmall te
  where
    viewFull  = TheoryEditor.view
    viewSmall = TheoryEditor.view -- TODO
