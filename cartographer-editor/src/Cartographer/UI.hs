{-# LANGUAGE OverloadedStrings #-}
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
  { theoryEditor :: Collapsible.Model TheoryBuilder.Model
  -- ^ a single theory editor
  , proofBuilder :: ProofBuilder.Model
  -- ^ a single proof
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
  ProofBuilderAction a ->
    Model te (ProofBuilder.update a pw) -- TODO!

-- TODO: dont run toTheory on every view; store in ProofBuilder model
-- i.e. replace with Maybe (Theory Generator, ProofBuilder.Model)
view :: Model -> View Action
view (Model te pb) = div_ [class_ "container is-widescreen"]
  [ subtitle "Theory"
  , TheoryBuilderAction <$> Collapsible.view viewFull viewSmall te
  , hr_ []
  , subtitle "Proof"
  , case TheoryBuilder.toTheory (Collapsible._modelInnerModel te) of
      Just t  -> ProofBuilderAction <$> ProofBuilder.view t pb
      Nothing -> div_ [] ["no proof"]
  ]
  where
    viewFull  = TheoryBuilder.view
    viewSmall = TheoryBuilder.view -- TODO
    subtitle s = h2_ [ class_ "subtitle is-2" ] [s]
    box = div_ [ class_ "box" ]
