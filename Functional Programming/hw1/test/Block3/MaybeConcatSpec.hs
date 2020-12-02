module Block3.MaybeConcatSpec (spec) where

import Block3.MaybeConcatTask (eitherConcat, maybeConcat)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "maybeConcat" $ do
    it "empty" $
      maybeConcat ([] :: [Maybe [Int]]) `shouldBe` []
    it "Nothing" $
      maybeConcat ([Nothing, Nothing, Nothing] :: [Maybe [Int]]) `shouldBe` []
    it "Just" $
      maybeConcat [Just [3, 4, 5]] `shouldBe` [3, 4, 5]
    it "Justs" $
      maybeConcat [Just [3, 4, 5], Just [1], Just [3, 4, 5]] `shouldBe` [3, 4, 5, 1, 3, 4, 5]
    it "mixed" $
      maybeConcat [Just [3, 4, 5], Nothing, Nothing, Just [3, 4, 5], Just [3, 4, 5], Nothing] `shouldBe` [3, 4, 5, 3, 4, 5, 3, 4, 5]

  describe "eitherConcat" $ do
    it "empty" $
      eitherConcat ([] :: [Either [Int] [Int]]) `shouldBe` ([], [])
    it "empty" $
      eitherConcat ([Left [1], Left [4, 5]] :: [Either [Int] [Int]]) `shouldBe` ([1, 4, 5], [])
    it "both present" $
      eitherConcat [Left [1], Right [2], Right [3], Left [4, 5]] `shouldBe` ([1, 4, 5], [2, 3])
    it "both present, diff types" $
      eitherConcat [Left [1], Right "hi", Right "bye", Left [4, 5]] `shouldBe` ([1, 4, 5], "hibye")
