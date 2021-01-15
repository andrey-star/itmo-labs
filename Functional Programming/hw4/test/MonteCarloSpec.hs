module MonteCarloSpec
  ( spec,
  )
where

import MonteCarloTask (monteCarlo)
import Test.Hspec (Spec, describe, it, shouldSatisfy)

spec :: Spec
spec = do
  describe "monteCarlo" $ do
    it "0.5 1 ~= 0.537375" $
      delta 10 `shouldSatisfy` (< 0.1) 
    it "converges" $ do
      delta 100 `shouldSatisfy` (< delta 10)
      delta 1000 `shouldSatisfy` (< delta 100)
      delta 10000 `shouldSatisfy` (< delta 1000)

    where
      delta :: Int -> Double
      delta n = abs (0.537375 - monteCarlo 0.5 1 n) 