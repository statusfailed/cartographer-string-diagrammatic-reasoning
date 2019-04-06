{-# LANGUAGE OverloadedStrings #-}
-- | ProofBuilder is a (stateful) component that capture the need to first
-- construct a valid morphism before a Proof can be begun.
-- 
-- Two stages:
--    1) Construct the "left hand side" of the proof
--    2) When valid, turn it into a Proof and behave as ProofAssistant
module Cartographer.Components.ProofBuilder where

import Miso

import Data.Foldable

import qualified Data.Hypergraph as Hypergraph
import qualified Cartographer.Layout as Layout
import qualified Cartographer.Proof as Proof

import Cartographer.Viewer (Generator(..))
import qualified Cartographer.Editor as Editor
import qualified Cartographer.Components.ProofAssistant as ProofAssistant

-- Are we building the first term of the model, or proving something?
data Model
  = Preproof Editor.Model
  | Proving  ProofAssistant.Model
  deriving(Eq, Ord, Show)

emptyModel :: Model
emptyModel = Preproof Editor.emptyModel

data Action
  = PreproofAction Editor.Action
  | ProvingAction  ProofAssistant.Action
  | BeginProof
  deriving(Eq, Ord, Show)

-- NOTE: bug here? what if you press "begin proof" before a theory is
-- available...
-- TODO: rethink this? big ole case switch for BeginProof seems weird
-- but it's late and it worked.... :)
update :: Action -> Model -> Model
update (PreproofAction a) (Preproof m) = Preproof $ Editor.update a m
update (ProvingAction a) (Proving m) = Proving $ ProofAssistant.update a m
update BeginProof (Preproof m) =
  -- TODO: this should probably be in Layout...?
  case Hypergraph.isComplete (Layout.hypergraph layout) of
    True  -> case (ProofAssistant.newModel layout) of
      Just pam -> Proving pam
      Nothing  -> Preproof m
    False -> Preproof m
  where layout = Editor._modelLayout m
update _ m = m

view :: Proof.Theory Generator -> Model -> View Action
view theory (Preproof m) = Miso.div_ []
  [ PreproofAction <$> Editor.view (toList $ Proof._theoryGenerators theory) m
  , Miso.button_ [onClick BeginProof] [ "begin proof" ]
  ]
view theory (Proving m) =
  ProvingAction <$> ProofAssistant.view theory m
