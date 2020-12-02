module Block3.BasicCombinatorsSpec
  ( spec,
  )
where

import Block3.BasicCombinatorsTask (element, eof, ok, satisfy, stream)
import Block3.ParserCombinatorTask (runParser)
import Data.Maybe (isJust, isNothing)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
  describe "ok" $ do
    it "empty input" $
      runParser ok "" `shouldSatisfy` isJust
    it "some input" $
      runParser ok "1834b f" `shouldSatisfy` isJust

  describe "eof" $ do
    it "empty input" $
      runParser eof "" `shouldSatisfy` isJust
    it "some input" $
      runParser eof "1834b f" `shouldSatisfy` isNothing

  describe "satisfy" $ do
    it "equals" $
      runParser (satisfy even) [2] `shouldBe` Just (2, [])
    it "starts with" $
      runParser (satisfy even) [2, 3, 4] `shouldBe` Just (2, [3, 4])
    it "doesnt start with" $
      runParser (satisfy even) [1, 2, 3] `shouldSatisfy` isNothing
    it "empty input" $
      runParser (satisfy even) [] `shouldSatisfy` isNothing

  describe "element" $ do
    it "equals" $
      runParser (element 'a') "a" `shouldBe` Just ('a', "")
    it "starts with" $
      runParser (element 'a') "absad" `shouldBe` Just ('a', "bsad")
    it "doesnt start with" $
      runParser (element 'a') "cabsad" `shouldSatisfy` isNothing
    it "empty input" $
      runParser (element 'a') "" `shouldSatisfy` isNothing

  describe "stream" $ do
    it "full match" $
      runParser (stream "abc") "abc" `shouldBe` Just ("abc", "")
    it "prefix match" $
      runParser (stream "abc") "abcdef" `shouldBe` Just ("abc", "def")
    it "partial match" $
      runParser (stream "abc") "abd" `shouldSatisfy` isNothing
