module Block2.ArithmeticSpec
  ( spec,
  )
where

import Block2.ArithmeticTask (ArithmeticError (..), Expr (..), eval)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "valid" $ do
    it "1+2=3" $
      eval (Add (Const 1) (Const 2)) `shouldBe` Right 3
    it "(5+6)*(1+9)=110" $
      eval (Mul (Add (Const 5) (Const 6)) (Add (Const 1) (Const 9))) `shouldBe` Right 110
    it "(5+6)/(1-(-9))=1" $
      eval (Div (Add (Const 5) (Const 6)) (Sub (Const 1) (Const (-9)))) `shouldBe` Right 1
    it "2^(1+9)=1024" $
      eval (Pow (Const 2) (Add (Const 1) (Const 9))) `shouldBe` Right 1024
    it "1024^0=1" $
      eval (Pow (Const 1024) (Const 0)) `shouldBe` Right 1

  describe "invalid" $ do
    it "1/0 = div by 0" $
      eval (Div (Const 1) (Const 0)) `shouldBe` Left DivisionByZero
    it "20 / (10 - 10) = div by 0" $
      eval (Div (Const 20) (Sub (Const 10) (Const 10))) `shouldBe` Left DivisionByZero
    it "1^(-50) = neg exp" $
      eval (Pow (Const 1) (Const (-50))) `shouldBe` Left NegativeExponent
    it "20^(10 - 11) = neg exp" $
      eval (Pow (Const 20) (Sub (Const 10) (Const 11))) `shouldBe` Left NegativeExponent
