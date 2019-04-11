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
-- "Preproof" basically just lets the user tell the ProofAssistant what
-- the starting point of the proof is.
data Model
  = Preproof Editor.Model
  | Proving  ProofAssistant.Model
  deriving(Eq, Ord, Show)

emptyModel :: Model
emptyModel = Preproof Editor.emptyModel

data Action
  = PreproofAction Editor.Action
  | ProvingAction  ProofAssistant.Action
  | BeginProof -- ^ begin the proof with the edited term
  | ClearProof -- ^ restart the proof
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
update ClearProof _ = emptyModel
update _ m = m

view :: Proof.Theory Generator -> Model -> View Action
view theory state = div_ [ class_ "box" ]
  [ case state of
      Preproof m -> viewPreproof theory m
      Proving  m -> ProvingAction <$> ProofAssistant.view theory m
  , hr_ []
  , button_ [ class_ "button is-danger", onClick ClearProof ]
    [ "restart proof" ]
  ]

viewPreproof theory m = div_ [ class_ "box" ]
  [ PreproofAction <$> Editor.view (toList $ Proof._theoryGenerators theory) m
  , Miso.button_ [class_ "button is-primary", onClick BeginProof]
    [ "begin proof" ]
  ]
