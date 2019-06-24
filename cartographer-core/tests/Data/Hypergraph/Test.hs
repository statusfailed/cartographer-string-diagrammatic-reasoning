module Data.Hypergraph.Test
  ( module Data.Hypergraph
  , module Data.Hypergraph.Test.Arbitrary
  , module Type
  , mainTests
  , main
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.Hypergraph
import Data.Hypergraph.Test.Arbitrary
import Data.Hypergraph.Test.Type as Type
import Data.Hypergraph.Test.Algebraic as Algebraic
import Data.Hypergraph.Test.Match as Match

import Data.Time.Clock
import Control.Monad
import Control.Monad.Logic
import Data.List (foldl')

mainTests :: TestTree
mainTests = testGroup "Data.Hypergraph"
  [ Type.tests
  , Algebraic.tests
  , Match.tests
  ]

main :: IO ()
main = defaultMain mainTests

-- match a small-ish pattern in a large graph.
smallInLarge = do
  a <- generate (generateSized 10000 :: Gen (OpenHypergraph Generator))
  b <- generate (generateSized 3     :: Gen (OpenHypergraph Generator))
  c <- generate (generateSized 10000 :: Gen (OpenHypergraph Generator))
  print . take 1 $ matchAll b (a → b → c)

-------------------------------
-- intentionally constructed slow cases

randomSingleton :: IO (OpenHypergraph Generator)
randomSingleton = generate (singleton <$> arbitrary)

-- demonstrate some kind of space leak:
-- use like this:
-- (a, b, g) <- parts 10000
-- take 1 (match a g) -- slow
-- take 1 (match a g) -- fast
-- take 1 (match b g) -- slow
parts n = do
  p1 <- f n
  a  <- f 3
  p2 <- f n
  b  <- f 3
  p3 <- f n
  return $ (a, b, (((p1 → a) → p2) → b) → p3)
  where f k = foldl' (→) empty <$> replicateM k randomSingleton

-- very fast: slowcase 1 500 2
-- very slow: slowcase 0 500 2
slowcase pad k n = (pat, ctx) where
  -- a 1 → 1 generator
  g = singleton $ Generator 0 (1,1)

  -- m sequential copies of g,  g --> g --> ... -> g
  chain m = foldl (→) empty (replicate m g)

  ctx = chain (k * n + pad) -- why does this only work for nonzero pad?
  pat = foldl (<>) empty (replicate n $ chain k)

bar pad k n = not . Prelude.null . uncurry matchAll $ slowcase pad k n

badbench = getCurrentTime >>= go 1 where
  go k t = do
    t' <- getCurrentTime
    let r = bar 0 k 2
    putStrLn $ "r: " ++ show (r,k) ++ " Δt: " ++ show (diffUTCTime t' t)
    go (succ k) t'

-------------------------------
-- Weird matching behaviour!

twiceCase = matchAll hg (hg <> hg) where
  g x = singleton $ Generator x (0, 1)
  hg = g 0 <> g 1

selfCase = matchAll hg hg where
  g = singleton (Generator 0 (1, 1))
  hg = g <> g

-------------------------------
-- Space leak debugging

-- about 5ms to match a size-5 pattern in a 100K size graph.
bigSize = do
  t0 <- getCurrentTime
  a <- generate (generateSized 50000 :: Gen (OpenHypergraph Generator))
  p <- generate (generateSized 5 :: Gen (OpenHypergraph Generator))
  b <- generate (generateSized 50000 :: Gen (OpenHypergraph Generator))
  let hg = (a → p → b)
  t1 <- getCurrentTime
  print (diffUTCTime t1 t0)

  print (graphSize hg)
  t2 <- getCurrentTime
  print (diffUTCTime t2 t1)

  putStrLn $ "finding pattern size: " ++ show (hypergraphSize p)
  print (observe $ match p hg)
  t3 <- getCurrentTime
  print (diffUTCTime t3 t2)

composePerf = do
  singletons <- replicateM 10000 randomSingleton
  let silly (a,b) (c,d) = (a+c, b+d)
  print $ foldl' silly (0,0) (toSize <$> singletons)
  t0 <- getCurrentTime

  -- NOTE: foldr here will just never finish XD
  let g1 = foldl' (→) empty singletons
  t1 <- getCurrentTime
  print (diffUTCTime t1 t0)

  print (graphSize g1)
  t2 <- getCurrentTime
  print (diffUTCTime t2 t1)

funPerf = do
  t0 <- getCurrentTime
  a <- generate (generateSized 100 :: Gen (OpenHypergraph Generator))
  b <- generate (generateSized 100 :: Gen (OpenHypergraph Generator))
  t1 <- getCurrentTime
  print (diffUTCTime t1 t0)

  let c = a → b
  print (graphSize c)
  t2 <- getCurrentTime
  print (diffUTCTime t2 t1)

  print (take 1 $ matchAll b c)
  t3 <- getCurrentTime
  print (diffUTCTime t3 t2)
