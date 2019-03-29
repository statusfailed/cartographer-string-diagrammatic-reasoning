module Model where

-- fmap foo view
foo :: MonadReader Model m => Position -> m ClickedTile
foo v@(V2 x y) = ClickedTile v $ case odd x of
  True  -> Right (clickedGenerator v)
  False -> Left (clickedWires v)

-------------------------------
-- TODO: rethink the below two functions;
-- they require that:
--  a) clickedGenerator => x is Odd
--  b) clickedWires     => x is Even

clickedGenerator :: MonadReader Model m -> Position -> m ClickedGenerator
clickedGenerator (V2 x y) = 

-- Given a position, make a ClickedWires.
clickedWires :: Position -> m ClickedWires
clickedWires (V2 x y) = ClickedWires l r
  where
    l = (x - 1) `div` 2
    r = (x + 1) `div` 2

