module Block1.NonEmptyInstanceSpec
  ( spec,
  )
where

import Block1.NonEmptyInstanceTask (NonEmpty (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "functor" $ do
    it "+1" $
      fmap (+ 1) one `shouldBe` 2 :| []
    it "*2" $
      fmap (* 2) oneFive `shouldBe` 2 :| [4, 6, 8, 10]
    it "identity law" $
      fmap id oneFive `shouldBe` oneFive
    it "composition law" $
      fmap ((+ 1) . (* 2)) oneFive `shouldBe` (fmap (+ 1) . fmap (* 2)) oneFive

  describe "applicative" $ do
    it "identity law" $
      pure id <*> oneFive `shouldBe` oneFive
    it "composition law" $
      pure (.) <*> plusOne <*> (+ 2) :| [] <*> 1 :| [] `shouldBe` plusOne <*> ((+ 2) :| [] <*> 1 :| [])
    it "homomorphism law" $
      (pure (+ 1) :: NonEmpty (Int -> Int)) <*> pure 1 `shouldBe` pure 2
    it "interchange law" $
      plusOne <*> pure 1 `shouldBe` pure ($ 1) <*> (+ 1) :| []

  describe "foldable" $ do
    it "sum" $
      foldr (+) 0 oneFive `shouldBe` 15

  describe "traversable" $ do
    it "list" $
      traverse (: []) oneFive `shouldBe` [oneFive]

  describe "monad" $ do
    it "+-1" $
      (two >>= (\x -> x :| [- x])) `shouldBe` 1 :| [-1, 2, -2]
    it "left identity law" $
      ((return 1 :: NonEmpty Int) >>= (\x -> x :| [-x])) `shouldBe` (\x -> x :| [-x]) 1
    it "right identity law" $
      (oneFive >>= return) `shouldBe` oneFive
    it "associativity law" $
      (oneFive >>= (\x -> k x >>= h)) `shouldBe` ((oneFive >>= k) >>= h)
  where
    one = 1 :| [] :: NonEmpty Int
    two = 1 :| [2] :: NonEmpty Int
    oneFive = 1 :| [2, 3, 4, 5] :: NonEmpty Int
    plusOne = (+ 1) :| [] :: NonEmpty (Int -> Int)
    k = \x -> x :| [- x]
    h = \x -> x :| [x]
