-- | This module contains a parallel implementation
-- of the monte-carlo integrating algorithm
-- for f = 1/tg(x^2) - cos(x)
module MonteCarloTask (monteCarlo, f) where

import Control.Parallel.Strategies (rpar, runEval)

-- | Returns the integral of 'f' from a to b.
monteCarlo :: Double -> Double -> Int -> Double
monteCarlo a b n = (b - a) / fromIntegral n * monteCarloParallel a b n

monteCarloParallel :: Double -> Double -> Int -> Double
monteCarloParallel a b 1 = f ((a + b) / 2)
monteCarloParallel a b n = runEval $ do
  let mid = (a + b) / 2
  let center = if even n then 0 else f mid
  let n2 = n `div` 2
  l <- rpar $ monteCarloParallel a mid n2
  r <- rpar $ monteCarloParallel mid b n2
  return $ l + center + r

-- | The function to be integrated.
f :: Double -> Double
f x = 1 / tan (x * x) - cos x
