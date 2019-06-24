module Main where

import Data.Hypergraph
import Data.List (foldl')
import Data.Time.Clock

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
      r = foldl' (→) empty (replicate 2000 $ c → a)
      g = r → b → r
  t0 <- getCurrentTime
  putStrLn ("mark: " ++ show t0)

  print $ toSize g
  t1 <- getCurrentTime
  putStrLn ("mark: " ++ show t1)

  print $ take 1 (match b g)
  t2 <- getCurrentTime
  putStrLn ("mark: " ++ show t2)

  print (diffUTCTime t2 t1)

main = matchProfile
{-main = tensorProfile-}
