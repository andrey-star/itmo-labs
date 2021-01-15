module Main where

import GeometryBenchmark (doubleAreaBenchmark, perimeterBenchmark)
import MonteCarloBenchmark (monteCarloBenchmark)
import Criterion.Main (defaultMain)

main :: IO ()
main = defaultMain =<< sequence [doubleAreaBenchmark 1000000, perimeterBenchmark 1000000, monteCarloBenchmark 10000000]
