{-# LANGUAGE OverloadedStrings #-}
module Cartographer.Components.TheoryBuilder where

import Miso
import Data.Foldable

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Cartographer.Proof as Proof

import Cartographer.Viewer (Generator(..))
import qualified Cartographer.Viewer as Viewer
import qualified Cartographer.Editor as Editor

import qualified Cartographer.Components.Sequence as Sequence
import qualified Cartographer.Components.GeneratorEditor as GeneratorEditor
import qualified Cartographer.Components.RuleBuilder as RuleBuilder

import qualified Cartographer.Preload as Preload

data Model = Model
  { signature :: Sequence.Model GeneratorEditor.Model
  , rules     :: Sequence.Model RuleBuilder.Model
  } deriving(Eq, Ord, Show)

emptyModel :: Model
emptyModel = Model Sequence.emptyModel Sequence.emptyModel

data Action
  = SignatureAction (Sequence.Action GeneratorEditor.Action)
  | RuleAction      (Sequence.Action RuleBuilder.Action)
  | Preload         (Proof.Theory Generator)
  deriving(Eq, Ord, Show)

update :: Action -> Model -> Model
update (SignatureAction a) (Model s r) =
  let s'= Sequence.update GeneratorEditor.emptyModel GeneratorEditor.update a s
  in  Model s' r
update (RuleAction a) (Model s r) =
  let r' = Sequence.update RuleBuilder.emptyModel RuleBuilder.update a r
  in  Model s r'
update (Preload theory) _ = fromTheory theory

view :: Model -> View Action
view (Model s r) = Miso.div_ []
  [ button_ [ class_ "button is-primary", onClick (Preload Preload.bialgebra) ]
    [ "preload example theory" ]
  , br_ []
  , box
    [ subtitle "signature"
    , Miso.div_ [] [ SignatureAction <$> Sequence.view GeneratorEditor.view  s ]
    ]
  , box
    [ subtitle "rewrite rules"
    , Miso.div_ [] [ RuleAction <$> Sequence.view (RuleBuilder.view gs) r ]
    ]
  ]
  where
    -- TODO: also include the set of generators already used in diagrams, OR
    -- don't permit editing generators used?
    gs = toGenerators s
    subtitle s = h4_ [ class_ "subtitle is-4" ] [ s ]
    box = div_ [ class_ "box" ]

toGenerators :: Sequence.Model GeneratorEditor.Model -> [Generator]
toGenerators (Sequence.Model gs) = toList gs

-- | Extract a 'Theory' from a TheoryBuilder 'Model'
toTheory :: Model -> Maybe (Proof.Theory Generator)
toTheory (Model (Sequence.Model sigs) (Sequence.Model pairs)) = do
  rules <- mapM RuleBuilder.toRule (toList pairs)
  return $ Proof.Theory (Set.fromList $ toList sigs) rules

fromTheory :: Proof.Theory Generator -> Model
fromTheory (Proof.Theory generators axioms) = Model gs as
  where
    gs = Sequence.fromList (toList generators)
    as = Sequence.fromList (fmap RuleBuilder.fromRule axioms)
