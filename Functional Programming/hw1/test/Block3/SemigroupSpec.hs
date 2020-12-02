module Block3.SemigroupSpec (spec) where

import Block3.SemigroupTask (Endo (..), Name (..), NonEmpty (..),
                             ThisOrThat (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "NonEmpty" $ do
    it "single" $
      1 :| [] <> 2 :| [] `shouldBe` 1 :| [2]
    it "multi" $
      1 :| [2, 3] <> 1 :| [2] <> 3 :| [1, 2, 3] `shouldBe` 1 :| [2, 3, 1, 2, 3, 1, 2, 3]

  describe "ThisOrThat" $ do
    it "assoc Both" $
      isAssociative (Both 1 2) (Both 3 4) (Both 5 6) `shouldBe` True
    it "mix" $
      isAssociative (Both 1 2) (Both 5 6) (This 4) `shouldBe` True
    it "mix" $
      isAssociative (Both 1 2) (This 4) (Both 5 6) `shouldBe` True
    it "mix" $
      isAssociative (This 4) (Both 1 2) (Both 5 6) `shouldBe` True
    it "mix" $
      isAssociative (Both 1 2) (This 4) (This 5) `shouldBe` True
    it "mix" $
      isAssociative (Both 1 2) (This 4) (That 5) `shouldBe` True
    it "mix" $
      isAssociative (That 1) (Both 1 4) (That 5) `shouldBe` True
    it "mix" $
      isAssociative (That 1) (Both 1 4) (This 5) `shouldBe` True
    it "mix" $
      isAssociative (This 1) (This 4) (Both 5 6) `shouldBe` True
    it "mix" $
      isAssociative (This 1) (That 4) (Both 5 6) `shouldBe` True

  describe "Name" $ do
    it "concat with ." $
      Name "a" <> Name "b" <> Name "c" `shouldBe` Name "a.b.c"
    it "concat with empty" $
      Name "" <> Name "a" <> Name "" <> Name "b" <> Name "" <> Name "c" `shouldBe` Name "a.b.c"
    it "associativity law" $
      isAssociative (Name "a") (Name "b") (Name "c") `shouldBe` True

  describe "Endo" $ do
    it "mempty" $
      getEndo (mempty :: Endo String) "hello" `shouldBe` "hello"
    it "compose" $
      getEndo (Endo (+ 1) <> Endo (+ 2) <> Endo (+ 3)) 0 `shouldBe` 6
    it "compose with empty" $
      getEndo (Endo (+ 1) <> mempty <> Endo (+ 3)) 0 `shouldBe` 4
    it "associativity law" $
      getEndo (Endo (+ 1) <> (Endo (+ 2) <> Endo (+ 3))) 0 `shouldBe` getEndo ((Endo (+ 1) <> Endo (+ 2)) <> Endo (+ 3)) 0
  where
    isAssociative :: (Semigroup a, Eq a) => a -> a -> a -> Bool
    isAssociative a b c = (a <> (b <> c)) == ((a <> b) <> c)
