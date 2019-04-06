{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor     #-}
module Cartographer.Components.Sequence where

import Data.Foldable (toList)

import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as Seq

import Miso

data Model m = Model (Seq m)
  deriving(Eq, Ord, Show)

data Action a = InnerAction Int a | Append

update :: m -> (a -> m -> m) -> Action a -> Model m -> Model m
update _ updateInner (InnerAction ix a) (Model values) =
  Model $ Seq.adjust (updateInner a) ix values
update emptyModel _ Append (Model values) = Model (values |> emptyModel)

-- TODO: parametrise by class name?
view :: (m -> View a) -> Model m -> View (Action a)
view viewInner (Model xs) = Miso.div_ [] $
  (zipWith viewOne [0..] (toList xs))
  ++ [ Miso.button_ [onClick Append] ["+"] ]
  where
    viewOne ix m = InnerAction ix <$> viewInner m
