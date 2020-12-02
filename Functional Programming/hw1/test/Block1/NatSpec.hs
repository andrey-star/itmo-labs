module Block1.NatSpec (spec) where

import Block1.NatTask (Nat (..), isEven, natDiv, natMod, natToInteger)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "fromInteger" $ do
    it "0" $
      zero `shouldBe` Z
    it "1" $
      one `shouldBe` S Z
    it "five" $
      five `shouldBe` S (S $ S $ S $ S Z)

  describe "natToInteger" $ do
    it "0" $
      natToInteger zero `shouldBe` 0
    it "11" $
      natToInteger eleven `shouldBe` 11
    it "30" $
      natToInteger thirty `shouldBe` 30

  describe "==" $ do
    it "should be true if equal" $
      five == five `shouldBe` True
    it "should be false if not equal" $
      five == one `shouldBe` False

  describe "cmp" $ do
    it "<= should be true if equal" $
      five <= five `shouldBe` True
    it "< should be false if equal" $
      five < five `shouldBe` False
    it ">= should be true if greater" $
      six >= five `shouldBe` True
    it "2 < 5" $
      two < five `shouldBe` True

  describe "+" $ do
    it "1 + 0 should be 1" $
      one + zero `shouldBe` S Z
    it "5 + 6 should be 11" $
      five + six `shouldBe` eleven

  describe "-" $ do
    it "1 - 0 should be 1" $
      one - zero `shouldBe` one
    it "5 - 6 should be 0" $
      five - six `shouldBe` Z
    it "14 - 5 should be 9" $
      fourteen - five `shouldBe` nine

  describe "*" $ do
    it "1 * 0 should be 0" $
      one * zero `shouldBe` zero
    it "5 * 1 should be 5" $
      five * one `shouldBe` five
    it "5 * 6 should be 30" $
      five * six `shouldBe` thirty

  describe "isEven" $ do
    it "0 should be even" $
      isEven zero `shouldBe` True
    it "1 should be odd" $
      isEven one `shouldBe` False
    it "11 should be odd" $
      isEven eleven `shouldBe` False
    it "30 should be even" $
      isEven thirty `shouldBe` True

  describe "div" $ do
    it "by 1 should be same" $
      natDiv eleven one `shouldBe` eleven
    it "11/2 should be 5" $
      natDiv eleven two `shouldBe` five
    it "30/5 should be 6" $
      natDiv thirty five `shouldBe` six
    it "5/30 should be 0" $
      natDiv five thirty `shouldBe` zero
      
  describe "mod" $ do
      it "by 1 should be zero" $
        natMod eleven one `shouldBe` zero
      it "11 % 2 should be 1" $
        natMod eleven two `shouldBe` one
      it "30/5 should be 0" $
        natMod thirty five `shouldBe` zero
      it "5/30 should be 5" $
        natMod five thirty `shouldBe` five
  where
    zero = fromInteger 0 :: Nat
    one = fromInteger 1 :: Nat
    two = fromInteger 2 :: Nat
    five = fromInteger 5 :: Nat
    six = fromInteger 6 :: Nat
    nine = fromInteger 9 :: Nat
    eleven = fromInteger 11 :: Nat
    fourteen = fromInteger 14 :: Nat
    thirty = fromInteger 30 :: Nat
