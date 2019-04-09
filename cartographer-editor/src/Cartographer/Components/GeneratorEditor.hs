{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE OverloadedLists    #-}
-- | View and edit a signature - a set of generators
module Cartographer.Components.GeneratorEditor where

import Miso
import qualified Miso as Miso
import qualified Miso.Svg as Svg
import Miso.String (MisoString, fromMisoString, ms)

import Cartographer.Layout (Layout(..))
import qualified Cartographer.Layout as Layout

-- hmmmm
import Cartographer.Viewer (Generator(..))
import qualified Cartographer.Viewer as Viewer
import qualified Cartographer.Viewer.Types as Viewer

type Model = Generator

emptyModel = Generator
  { size  = (1, 1) -- can this be (0, 0) without bugs? :)
  , ports = ([0], [0]) -- comma sep list?
  , color = "#000000"
  , name  = "generator"
  }

data Action
  = NoOp
  | ValidationError MisoString -- ^ hacky!
  | SetColor        MisoString
  | SetName         MisoString
  | SetNumInput     Int
  | SetNumOutput    Int
  deriving(Eq, Ord, Read, Show)
  -- TODO
  {-| SetInputPorts   [Int]-}
  {-| SetOutputPorts  [Int]-}

update :: Action -> Model -> Model
update a m = case a of
  NoOp          -> m
  SetColor c    -> m { color = c }
  SetName  s    -> m { name = s }
  -- TODO: configurable number of ports
  SetNumInput k ->
    let (_, n) = size m
    in  updatePorts (k, n) m
  SetNumOutput n ->
    let (k, _) = size m
    in  updatePorts (k, n) m

updatePorts :: (Int, Int) -> Generator -> Generator
updatePorts (k, n) g = g
  { size  = (k, n)
  , ports = (mkPortList k h, mkPortList n h)
  }
  where
    h = Viewer.tileHeight k n

-- | Automatically pick a nice symmetric list of port offsets
mkPortList :: Int -> Int -> [Int]
mkPortList n h = left ++ mid ++ right
  where
    m     = n `div` 2
    left  = [0..m - 1]
    right = fmap (\x -> h - 1 - x) left
    mid   = if odd n then [h `div` 2] else []

view :: Generator -> View Action
view g = div_ [ class_ "message is-info" ]
  [ div_ [ class_ "message-header" ]
    [ "Generator"
    , button_ [ class_ "delete" ] [] -- TODO hook this up
    ]
  , div_ [ class_ "message-body" ]
    [ div_ [ class_ "columns" ]
      [ col . box $ editControls g
      , col . box $ previewGenerator g
      ]
    ]
  ]
  where
    col = div_ [ class_ "column" ] . pure
    box = div_ [ class_ "box" ] . pure

editControls :: Generator -> View Action
editControls g = div_ []
  [ SetName              <$> horizontalInput "Name" (name g)
  , numeric SetNumInput  <$> horizontalInput "# inputs" (ms . fst . size $ g)
  , numeric SetNumOutput <$> horizontalInput "# outputs" (ms . snd . size $ g)
  , SetColor             <$> horizontalInput "Color" (color g)
  ]

horizontalInput label value =
  div_ [ class_ "field is-horizontal" ]
    [ fieldLabel label
    , fieldBody value
    ]

fieldLabel label = 
  div_ [ class_ "field-label is-normal" ]
    [ label_ [ class_ "label" ] [ label ] ]

fieldBody value =
  div_ [ class_ "field-body" ]
    [ div_ [ class_ "field" ]
      [ input_ [ class_ "input", onInput id, value_ value ] ]
    ]

numeric :: (Int -> Action) -> MisoString -> Action
numeric f s = case reads (fromMisoString s) of
  [] -> NoOp 
  (x,_):_ -> f x

inlineInput :: [Attribute action] -> View action
inlineInput attrs = input_ (style_ [("display", "inline-block")] : attrs)

previewGenerator :: Generator -> View action
previewGenerator g = div_ []
  [ h5_ [] [ "preview: ", text . ms . name $ g ]
  , Svg.svg_ attrs [ Viewer.viewGenerator g 0 opts ]
  ]
  where
    opts = Viewer.defaultOptions
    height = fromIntegral (Layout.generatorHeight g)
    attrs =
      [ Svg.height_ (ms $ height * Viewer.tileSize opts)
      , Svg.width_  (ms $ Viewer.tileSize opts)
      ]

-------------------------------
-- utilities

isStrictlyMonotonic :: Ord a => [a] -> Bool
isStrictlyMonotonic [] = True
isStrictlyMonotonic (x:xs) = go x xs
  where
    go x []      = True
    go x (x':xs) = x' > x && go x' xs
