module Block2.SimpleMovingAverageSpec
  ( spec,
  )
where

import Block2.SimpleMovingAverageTask (moving)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "moving" $ do
    it "window = 1" $
      moving 1 [1, 5, 3, 8, 7, 9, 6] `shouldBe` [1.0, 5.0, 3.0, 8.0, 7.0, 9.0, 6.0]
    it "moving 2 [1, 5, 3, 8, 7, 9, 6]" $
      moving 2 [1, 5, 3, 8, 7, 9, 6] `shouldBe` [1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]
    it "4 [1, 5, 3, 8, 7, 9, 6]" $
      moving 4 [1, 5, 3, 8, 7, 9, 6] `shouldBe` [1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5]
    it "window bigger than array" $
      moving 40 [1, 5, 3, 8, 7, 9] `shouldBe` [1.0, 3.0, 3.0, 4.25, 4.8, 5.5]
    it "large with window = 1" $
      moving 1 [1,2..1000] `shouldBe` ([1,2..1000] :: [Float])
