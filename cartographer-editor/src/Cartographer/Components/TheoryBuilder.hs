{-# LANGUAGE DeriveFunctor #-}
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

data Model = Model
  { signature :: Sequence.Model GeneratorEditor.Model
  , rules     :: Sequence.Model RuleBuilder.Model
  } deriving(Eq, Ord, Show)

emptyModel :: Model
emptyModel = Model Sequence.emptyModel Sequence.emptyModel

data Action
  = SignatureAction (Sequence.Action GeneratorEditor.Action)
  | RuleAction      (Sequence.Action RuleBuilder.Action)
  deriving(Eq, Ord, Show)

update :: Action -> Model -> Model
update (SignatureAction a) (Model s r) =
  let s'= Sequence.update GeneratorEditor.emptyModel GeneratorEditor.update a s
  in  Model s' r
update (RuleAction a) (Model s r) =
  let r' = Sequence.update RuleBuilder.emptyModel RuleBuilder.update a r
  in  Model s r'

view :: Model -> View Action
view (Model s r) = Miso.div_ []
  [ Miso.div_ [] [ SignatureAction <$> Sequence.view GeneratorEditor.view  s ]
  , Miso.div_ [] [ RuleAction <$> Sequence.view (RuleBuilder.view gs) r ]
  ]
  where
    -- TODO: also include the set of generators already used in diagrams, OR
    -- don't permit editing generators used?
    gs = toGenerators s

toGenerators :: Sequence.Model GeneratorEditor.Model -> [Generator]
toGenerators (Sequence.Model gs) = toList gs

-- | Extract a 'Theory' from a TheoryBuilder 'Model'
toTheory :: Model -> Maybe (Proof.Theory Generator)
toTheory (Model (Sequence.Model sigs) (Sequence.Model pairs)) = do
  rules <- mapM RuleBuilder.toRule (toList pairs)
  return $ Proof.Theory (Set.fromList $ toList sigs) rules
