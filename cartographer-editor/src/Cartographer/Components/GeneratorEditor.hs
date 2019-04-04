{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE OverloadedLists    #-}
-- | View and edit a signature - a set of generators
module Cartographer.Components.GeneratorEditor where

import Miso (View(..), Attribute)
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
view g = Miso.div_ [Miso.style_ [("display", "flex")]]
  [ editControls g
  , previewGenerator g
  ]

editControls :: Generator -> View Action
editControls g = Miso.div_ [Miso.style_ [("flex", "50%")]]
  [ Miso.input_
      [ Miso.placeholder_ "name"
      , Miso.onInput SetName
      , Miso.value_ (name g)
      ] -- name
  , Miso.div_ []
    [ inlineInput
        [ Miso.placeholder_ "# inputs"
        , Miso.onInput (numeric SetNumInput)
        , Miso.value_ (ms . fst . size $ g)
        ]
    , " -> "
    , inlineInput
        [ Miso.placeholder_ "# outputs"
        , Miso.onInput (numeric SetNumOutput)
        , Miso.value_ (ms . snd . size $ g)
        ]
    ]
  , Miso.input_
      [ Miso.placeholder_ "color"
      , Miso.onInput SetColor
      , Miso.value_ (color g)
      ]
  ]

numeric :: (Int -> Action) -> MisoString -> Action
numeric f s = case reads (fromMisoString s) of
  [] -> NoOp 
  (x,_):_ -> f x

inlineInput :: [Attribute action] -> View action
inlineInput attrs = Miso.input_ (Miso.style_ [("display", "inline-block")] : attrs)

previewGenerator :: Generator -> View action
previewGenerator g = Miso.div_ [Miso.style_ [("flex", "50%")]]
  [ Svg.svg_ attrs [ Viewer.viewGenerator g 0 opts ] ]
  where
    tileSize = 50
    opts = Viewer.ViewerOptions tileSize
    height = fromIntegral (Layout.generatorHeight g)
    attrs =
      [ Svg.height_ (ms $ height * tileSize)
      , Svg.width_  (ms tileSize)
      ]

-------------------------------
-- utilities

isStrictlyMonotonic :: Ord a => [a] -> Bool
isStrictlyMonotonic [] = True
isStrictlyMonotonic (x:xs) = go x xs
  where
    go x []      = True
    go x (x':xs) = x' > x && go x' xs
