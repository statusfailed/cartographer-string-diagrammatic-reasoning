{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE OverloadedLists    #-}
-- | Create a 'Proof'
module Cartographer.Components.ProofAssistant where

import Miso (View(..), Attribute)
import qualified Miso as Miso
import qualified Miso.Svg as Svg
import Miso.String (MisoString, fromMisoString, ms)

import Data.Hypergraph (MatchState(..))

import Cartographer.Layout (Layout(..))
import qualified Cartographer.Layout as Layout

import Cartographer.Proof (Proof(..), ProofStep(..), Rule, Theory(..))
import qualified Cartographer.Proof as Proof

-- hmmmm
import Cartographer.Viewer (Generator(..))
import qualified Cartographer.Viewer as Viewer
import qualified Cartographer.Viewer.Types as Viewer

type Model = Proof Generator

newModel :: Layout Generator -> Maybe Model
newModel = Proof.proof

data Action
  = NoOp
  | Apply (Rule Generator, MatchState Generator)
  | Undo
  deriving(Eq, Ord, Show)

update :: Action -> Model -> Model
update a m = case a of
  NoOp    -> m
  Undo    -> Proof.undo m
  Apply r -> Proof.applyRule r m

view :: Theory Generator -> Model -> View Action
view theory proof@(Proof _ steps) = Miso.div_ []
  [ Miso.div_ [] (viewStep <$> reverse steps)
  , const NoOp <$> Viewer.view (Proof._proofTerm proof) Viewer.defaultOptions
  , viewMatches theory proof
  , Miso.button_ [ Miso.onClick Undo ] [ "undo" ]
  ]

viewStep :: ProofStep Generator -> View Action
viewStep (ProofStep term rule match) = Miso.div_ []
  [ const NoOp <$> Viewer.viewWith match term Viewer.defaultOptions ]

viewMatches :: Theory Generator -> Model -> View Action
viewMatches theory proof = Miso.div_ [Miso.style_ [("display", "flex")]] $
  fmap viewRuleMatch (Proof.matchingRules theory (Proof._proofTerm proof))

viewRuleMatch :: (Rule Generator, MatchState Generator) -> View Action
viewRuleMatch x@(_, m) = Miso.div_ [Miso.style_ [("flex", "20%")]]
  [ Miso.button_ [Miso.onClick (Apply x)] [Miso.text . ms . show $ m] ]
