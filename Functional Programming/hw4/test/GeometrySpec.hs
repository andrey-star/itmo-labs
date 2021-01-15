module GeometrySpec
  ( spec,
  )
where

import GeometryTask (Point (..), crossProduct, doubleArea, minus, perimeter,
                     plus, scalarProduct)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "operators" $ do
    it "plus" $
      Point 1 2 `plus` Point 2 3 `shouldBe` Point 3 5
    it "plus" $
      Point 1 2 `plus` Point (-1) (-2) `shouldBe` Point 0 0
    it "minus" $
      Point 1 2 `minus` Point 2 3 `shouldBe` Point (-1) (-1)
    it "minus" $
      Point 1 2 `minus` Point 1 2 `shouldBe` Point 0 0
    it "scalarProduct" $
      Point 1 2 `scalarProduct` Point 2 3 `shouldBe` 8
    it "scalarProduct" $
      Point 1 2 `scalarProduct` Point 0 0 `shouldBe` 0
    it "crossProduct" $
      Point 1 2 `crossProduct` Point 2 3 `shouldBe` -1
    it "crossProduct" $
      Point 1 2 `crossProduct` Point 0 0 `shouldBe` 0

  describe "operators" $ do
    it "perimeter" $
      perimeter [Point 0 0] `shouldBe` 0
    it "perimeter" $
      perimeter [Point 0 0, Point 3 0] `shouldBe` 6
    it "perimeter" $
      perimeter [Point 0 0, Point 3 0, Point 3 4] `shouldBe` 12
    it "doubleArea" $
      doubleArea [Point 0 0] `shouldBe` 0
    it "doubleArea" $
      doubleArea [Point 0 0, Point 3 0] `shouldBe` 0
    it "doubleArea" $
      doubleArea [Point 0 0, Point 3 0, Point 3 4] `shouldBe` 12
    it "doubleArea" $
      doubleArea [Point 0 0, Point 3 0, Point 3 3, Point 0 3] `shouldBe` 18
