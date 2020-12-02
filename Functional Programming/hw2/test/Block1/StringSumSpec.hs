module Block1.StringSumSpec
  ( spec,
  )
where

import Block1.StringSumTask (stringSum)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "valid input" $ do
    it "empty" $
      stringSum "" `shouldBe` Just 0
    it "one number" $
      stringSum "10" `shouldBe` Just 10
    it "15" $
      stringSum "1 2 3 4 5" `shouldBe` Just 15
    it "100" $
      stringSum "55 45" `shouldBe` Just 100
    it "0" $
      stringSum "123 780 -780 -123" `shouldBe` Just 0

  describe "invalid input" $ do
    it "cant parse int" $
      stringSum "1 2 f 4 5" `shouldBe` Nothing
    it "cant parse int" $
      stringSum "jfgjdk" `shouldBe` Nothing
    it "cant parse int" $
      stringSum "34s 12 13" `shouldBe` Nothing
