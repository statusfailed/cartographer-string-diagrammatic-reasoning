{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor     #-}
module Cartographer.Components.Sequence where

import Data.Foldable (toList)

import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as Seq

import Miso

data Model m = Model (Seq m)
  deriving(Eq, Ord, Show)

emptyModel :: Model a
emptyModel = Model Seq.empty

data Action a = InnerAction Int a | Append
  deriving(Eq, Ord, Show)

update :: m -> (a -> m -> m) -> Action a -> Model m -> Model m
update _ updateInner (InnerAction ix a) (Model values) =
  Model $ Seq.adjust (updateInner a) ix values
update emptyInner _ Append (Model values) = Model (values |> emptyInner)

-- TODO: parametrise by class name?
view :: (m -> View a) -> Model m -> View (Action a)
view viewInner (Model xs) = Miso.div_ attrs
  [ Miso.button_ [onClick Append] ["+"]
  , Miso.div_ [] $ zipWith viewOne [0..] (toList xs)
  ]
  where
    viewOne ix m = InnerAction ix <$> viewInner m
    attrs = [ Miso.class_ "sequence" ]
