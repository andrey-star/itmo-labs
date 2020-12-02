module Block3.SimpleParsersSpec
  ( spec,
  )
where

import Block3.ParserCombinatorTask (runParser)
import Block3.SimpleParsersTask (balancedBrackets, int)
import Data.Maybe (isJust, isNothing)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
  describe "balancedBrackets" $ do
    describe "valid" $ do
      it "empty" $
        runParser balancedBrackets "" `shouldSatisfy` isJust
      it "simple" $
        runParser balancedBrackets "()" `shouldSatisfy` isJust
      it "cons" $
        runParser balancedBrackets "()()()" `shouldSatisfy` isJust
      it "nested" $
        runParser balancedBrackets "((()))" `shouldSatisfy` isJust
      it "mix" $
        runParser balancedBrackets "(()())()(())" `shouldSatisfy` isJust

    describe "invalid" $ do
      it "not closed" $
        runParser balancedBrackets "(" `shouldSatisfy` isNothing
      it "not closed" $
        runParser balancedBrackets "(((" `shouldSatisfy` isNothing
      it "not closed" $
        runParser balancedBrackets "(()()" `shouldSatisfy` isNothing
      it "not closed" $
        runParser balancedBrackets "()()(" `shouldSatisfy` isNothing
      it "not open" $
        runParser balancedBrackets ")" `shouldSatisfy` isNothing
      it "not open" $
        runParser balancedBrackets "())" `shouldSatisfy` isNothing
      it "not open" $
        runParser balancedBrackets ")(())" `shouldSatisfy` isNothing
      it "not open" $
        runParser balancedBrackets "((())))" `shouldSatisfy` isNothing

  describe "int" $ do
    describe "positive" $ do
      it "0" $
        runParser int "0" `shouldBe` Just (0, "")
      it "1" $
        runParser int "1" `shouldBe` Just (1, "")
      it "1456" $
        runParser int "1456" `shouldBe` Just (1456, "")
      it "with rest" $
        runParser int "1456abc" `shouldBe` Just (1456, "abc")

    describe "positive sign" $ do
      it "+0" $
        runParser int "+0" `shouldBe` Just (0, "")
      it "+1500" $
        runParser int "+1500" `shouldBe` Just (1500, "")
      it "+1500+" $
        runParser int "+1500+" `shouldBe` Just (1500, "+")

    describe "negative" $ do
      it "-0" $
        runParser int "-0" `shouldBe` Just (0, "")
      it "-1500" $
        runParser int "-1500" `shouldBe` Just (-1500, "")
      it "+1500+" $
        runParser int "-1500-" `shouldBe` Just (-1500, "-")

    describe "invalid" $ do
      it "+" $
        runParser int "+" `shouldSatisfy` isNothing
      it "-" $
        runParser int "-" `shouldSatisfy` isNothing
      it "abc" $
        runParser int "abc" `shouldSatisfy` isNothing
      it "ab13" $
        runParser int "ab13" `shouldSatisfy` isNothing
