{-# LANGUAGE DeriveFunctor #-}
module Cartographer.Components.Sequence where

import Data.Foldable (toList)
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq

import Miso

data Model m = Model (Seq m)
  deriving(Eq, Ord, Show)

data Action a = Action Int a

update :: (a -> m -> m) -> Action a -> Model m -> Model m
update updateInner (Action ix a) (Model values) =
  Model $ Seq.adjust (updateInner a) ix values

-- TODO: parametrise by class name?
view :: (m -> View a) -> Model m -> View (Action a)
view viewInner (Model xs) = Miso.div_ [] (zipWith viewOne [0..] (toList xs))
  where
    viewOne ix m = Action ix <$> viewInner m
