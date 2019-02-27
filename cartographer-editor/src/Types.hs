module Types where

import Miso.String (MisoString)
import Data.Hypergraph

data Generator = Generator
  { size :: (Int, Int)
  , ports :: ([Int], [Int])
  , color :: MisoString
  , name :: MisoString
  } deriving(Eq, Ord, Read, Show)

instance Signature Generator where
  toSize = size

-- | Height (in tiles) of the generator.
-- TODO: explain this better.
-- A generator of size (2,1) will have total height 3, so it looks symmetric.
tileHeight (Generator (l, r) _ _ _) = max l r + modifier
  where modifier = if l == 0 || r == 0 then 0 else mod (l+r) 2
