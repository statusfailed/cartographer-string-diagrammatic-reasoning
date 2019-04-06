-- Design
--
-- "*Builder" modules represent editors where the "editing" state might not
-- represent a finished product - e.g., Rules might be invalid until the left
-- diagram has the same number of inputs and outputs as the right.
module Cartographer.UI where

import Miso

import qualified Cartographer.Components.Collapsible as Collapsible
import qualified Cartographer.Components.Sequence as Sequence

import qualified Cartographer.Components.TheoryBuilder as TheoryBuilder
import qualified Cartographer.Components.ProofBuilder as ProofBuilder

import qualified Cartographer.Components.RuleBuilder as RuleBuilder

data Model = Model
  { theoryEditor :: Collapsible.Model TheoryBuilder.Model -- ^ a single theory editor
  , proofWrapper :: ProofBuilder.Model
  } deriving(Eq, Ord, Show)

emptyModel :: Model
emptyModel = Model
  (Collapsible.newModel TheoryBuilder.emptyModel)
  ProofBuilder.emptyModel

data Action
  = TheoryBuilderAction (Collapsible.Action TheoryBuilder.Action)
  | ProofBuilderAction ProofBuilder.Action
  deriving(Eq, Ord, Show)

update :: Action -> Model -> Model
update a (Model te pw) = case a of
  TheoryBuilderAction a ->
    Model (Collapsible.update TheoryBuilder.update a te) pw
  ProofBuilderAction a -> Model te pw -- TODO!

-- TODO: ProofBuilder
view :: Model -> View Action
view (Model te _) =
  TheoryBuilderAction <$> Collapsible.view viewFull viewSmall te
  where
    viewFull  = TheoryBuilder.view
    viewSmall = TheoryBuilder.view -- TODO
