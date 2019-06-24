module Main where

import Data.Hypergraph
import Data.List (foldl')
import Data.Time.Clock

data Generator = Generator (Int, Int)

instance Signature Generator where
  toSize (Generator x) = x

main = do
  let a = singleton (Generator (1,2))
      b = singleton (Generator (2,1))
      r = foldl' (→) empty (replicate 100000 $ a → b)
  t0 <- getCurrentTime
  putStrLn ("started: " ++ show t0)
  print $ toSize r
  t1 <- getCurrentTime
  putStrLn ("finished: " ++ show t1)
  print (diffUTCTime t1 t0)
