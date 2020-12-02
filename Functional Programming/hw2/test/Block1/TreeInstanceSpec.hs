module Block1.TreeInstanceSpec
  ( spec,
  )
where

import Block1.TreeInstanceTask (Tree (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "functor" $ do
    it "+1" $
      fmap (+ 1) oneTwo `shouldBe` Branch (Leaf 2) (Leaf 3)
    it "*2" $
      fmap (* 2) oneFour `shouldBe` Branch (Branch (Leaf 2) (Leaf 4)) (Branch (Leaf 6) (Leaf 8))
    it "identity law" $
      fmap id oneFour `shouldBe` oneFour
    it "composition law" $
      fmap ((+ 1) . (* 2)) oneFour `shouldBe` (fmap (+ 1) . fmap (* 2)) oneFour

  describe "applicative" $ do
    it "identity law" $
      pure id <*> oneFour `shouldBe` oneFour
    it "composition law" $
      pure (.) <*> Leaf (+ 1) <*> Leaf (+ 2) <*> Leaf 1 `shouldBe` Leaf (+ 1) <*> (Leaf (+ 2) <*> Leaf 1)
    it "homomorphism law" $
      (pure (+ 1) :: Tree (Int -> Int)) <*> pure 1 `shouldBe` pure 2
    it "interchange law" $
      Leaf (+ 1) <*> pure 1 `shouldBe` pure ($ 1) <*> Leaf (+ 1)

  describe "foldable" $ do
    it "sum" $
      foldr (+) 0 oneFour `shouldBe` 10
  
  describe "traversable" $ do
      it "list " $
        traverse (: []) oneFour `shouldBe` [oneFour]
  where
    oneTwo = Branch (Leaf 1) (Leaf 2) :: Tree Int
    oneFour = Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 3) (Leaf 4)) :: Tree Int
