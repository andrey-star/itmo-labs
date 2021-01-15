module GeometryBenchmark (doubleAreaBenchmark, perimeterBenchmark) where

import Control.DeepSeq (NFData)
import Criterion.Main (bench, bgroup, nf, Benchmark)
import GeometryTask (Point (..), crossProduct, doubleArea, perimeter, dist)
import Test.QuickCheck.Gen (Gen, choose, generate, vectorOf)

doubleAreaBenchmark :: Int -> IO Benchmark
doubleAreaBenchmark = benchmark "doubleArea" doubleArea doubleAreaLazy
  where
    doubleAreaLazy :: [Point] -> Int
    doubleAreaLazy ps = abs (go 0 ps)
      where
        go :: Int -> [Point] -> Int
        go _ [] = 0
        go acc [last_p] = acc + crossProduct last_p (head ps)
        go acc (p1 : p2 : pps) = go (acc + crossProduct p1 p2) (p2 : pps)

perimeterBenchmark :: Int -> IO Benchmark
perimeterBenchmark = benchmark "perimeter" perimeter perimeterLazy
  where
    perimeterLazy :: [Point] -> Double
    perimeterLazy ps = go 0 ps
      where
        go :: Double -> [Point] -> Double
        go _ [] = 0
        go acc [last_p] = acc + dist last_p (head ps)
        go acc (p1 : p2 : pps) = go (acc + dist p1 p2) (p2 : pps)

benchmark :: (NFData a) => String -> ([Point] -> a) -> ([Point] -> a) -> Int -> IO Benchmark
benchmark name p_opt p_lazy n = do
  list <- generate genPoints
  return $ bgroup name
    [ bgroup name [bench "lazy" $ nf p_lazy list],
      bgroup name [bench "optimized" $ nf p_opt list]
    ]
  where
    genPoints :: Gen [Point]
    genPoints = vectorOf n genPoint
      where
        genPoint :: Gen Point
        genPoint = do
          x <- choose (-1000, 1000)
          y <- choose (-1000, 1000)
          return (Point x y)
