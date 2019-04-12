{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE OverloadedLists    #-}
-- | Create a 'Proof'
module Cartographer.Components.ProofAssistant where

import Miso
  ( View(..), Attribute, div_, style_, class_
  , onMouseOver, onMouseLeave, onClick
  )
import qualified Miso as Miso
import qualified Miso.Svg as Svg
import Miso.String (MisoString, fromMisoString, ms)

import Data.Hypergraph (MatchState(..), emptyMatchState)

import Cartographer.Layout (Layout(..))
import qualified Cartographer.Layout as Layout

import Cartographer.Proof (Proof(..), ProofStep(..), Rule, Theory(..))
import qualified Cartographer.Proof as Proof

-- hmmmm
import Cartographer.Viewer (Generator(..))
import qualified Cartographer.Viewer as Viewer
import qualified Cartographer.Viewer.Types as Viewer

data Model = Model
  { _modelProof     :: Proof Generator
  , _modelHighlight :: MatchState Generator
  } deriving(Eq, Ord, Show)

newModel :: Layout Generator -> Maybe Model
newModel m = do
  p <- Proof.proof m
  return $ Model p emptyMatchState

data Action
  = NoOp
  | Apply        (Rule Generator, MatchState Generator)
  | SetHighlight (MatchState Generator)
  | Undo
  deriving(Eq, Ord, Show)

update :: Action -> Model -> Model
update a m@(Model p h) = case a of
  NoOp            -> m
  Undo            -> m { _modelProof = Proof.undo p }
  Apply r         -> m { _modelProof = Proof.applyRule r p }
  SetHighlight h  -> m { _modelHighlight = h }

view :: Theory Generator -> Model -> View Action
view theory (Model proof highlight) = Miso.div_ []
  [ Miso.div_ [] (viewStep <$> reverse steps)
  , Miso.div_ [ class_ "box current-term" ]
    [ const NoOp <$>
        Viewer.viewWith highlight (Proof._proofTerm proof) Viewer.defaultOptions
    , viewMatches theory proof
    , Miso.button_ [ class_ "button is-warning", Miso.onClick Undo ] [ "undo" ]
    ]
  ]
  where
    Proof _ steps = proof

viewStep :: ProofStep Generator -> View Action
viewStep (ProofStep term rule match) =
  Miso.div_ [ class_ "box step" ]
    [ const NoOp <$> Viewer.viewWith match term Viewer.defaultOptions ]

viewMatches :: Theory Generator -> Proof Generator -> View Action
viewMatches theory proof = Miso.div_ attrs $
  fmap miniRule (Proof.matchingRules theory (Proof._proofTerm proof))
  where attrs = [ style_ [("display", "flex"), ("flex-wrap", "wrap")] ]

-- | View a rule as a clickable button that also highlights the matched
-- subgraph of the current term when hovered.
miniRule :: (Rule Generator, MatchState Generator) -> View Action
miniRule rule@(Proof.Rule lhs rhs, m) =
  div_  [ style_ [("flex", "25%"), ("max-width", "25%")]
        , class_ "box"
        , onMouseOver (SetHighlight m)
        , onMouseLeave (SetHighlight emptyMatchState)
        , onClick (Apply rule)
        ]
    [ div_ [ style_ [("display", "flex")] ]
        [ flex50 [ const NoOp <$> Viewer.view lhs opts ]
        , flex50 [ const NoOp <$> Viewer.view rhs opts ]
        ]
    ]
  where
    flex50 xs = div_ [ style_ [("flex", "50%")] ] xs
    opts = Viewer.defaultOptions
      { Viewer.tileSize = 15
      , Viewer.showGrid = False
      }
