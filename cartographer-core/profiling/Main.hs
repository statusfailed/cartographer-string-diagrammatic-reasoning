module Main where

import Data.Hypergraph
import Control.Monad.Logic
import Data.List (foldl')
import Data.Time.Clock

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

graphSize :: OpenHypergraph sig -> (Int, Int)
graphSize g = (Bimap.size (connections g), Map.size (signatures g))

data Generator = Generator (Int, Int)
  deriving(Eq, Ord, Read, Show)

instance Signature Generator where
  toSize (Generator x) = x

profileOp n (·) = do
  let a = singleton (Generator (1,2))
      b = singleton (Generator (2,1))
      r = foldl' (·) empty (replicate n $ a → b)
  t0 <- getCurrentTime
  putStrLn ("started: " ++ show t0)
  print $ toSize r
  t1 <- getCurrentTime
  putStrLn ("finished: " ++ show t1)
  print (diffUTCTime t1 t0)

-------------------------------

composeProfile = profileOp 100000 (→)
tensorProfile = profileOp 100000 (<>)

-------------------------------

matchProfile = do
  let a = singleton (Generator (2,1))
      b = singleton (Generator (2,2))
      c = singleton (Generator (1,2))
      r = foldl' (→) empty (replicate 50000 $ c → a)
      p = c → b → a
      g = r → p → r
  t0 <- getCurrentTime
  putStrLn ("mark: " ++ show t0)

  print $ graphSize g
  t1 <- getCurrentTime
  putStrLn ("mark: " ++ show t1)

  print . observe $ match p g
  t2 <- getCurrentTime
  putStrLn ("mark: " ++ show t2)

  print (diffUTCTime t2 t1)

-------------------------------
-- small patterns in big graphs

main = matchProfile
{-main = tensorProfile-}
