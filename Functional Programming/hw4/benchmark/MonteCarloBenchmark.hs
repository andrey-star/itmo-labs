module MonteCarloBenchmark (monteCarloBenchmark) where

import Criterion.Main (bench, bgroup, nf, Benchmark)
import MonteCarloTask (monteCarlo, f)

monteCarloBenchmark :: Int -> IO Benchmark
monteCarloBenchmark = benchmark "monteCarlo" monteCarlo monteCarloLazy
  where
    monteCarloLazy :: Double -> Double -> Int -> Double
    monteCarloLazy a b n = sum (map f (linspace a b n)) / fromIntegral n * (b - a)
    
    linspace :: Double -> Double -> Int -> [Double]
    linspace a b n
      | n == 0 = []
      | n == 1 = [(a + b) / 2]
      | otherwise = map (\i -> a + fromIntegral i * inc) [0 .. (n - 1)]
      where
        inc = (b - a) / fromIntegral (n - 1)

benchmark :: String -> (Double -> Double -> Int -> Double) -> (Double -> Double -> Int -> Double) -> Int -> IO Benchmark
benchmark name p_opt p_lazy n = do
  return $ bgroup name
    [ bgroup name [bench "lazy" $ nf (p_lazy 0.1 1) n],
      bgroup name [bench "optimized" $ nf (p_opt 0.1 1) n]
    ]
