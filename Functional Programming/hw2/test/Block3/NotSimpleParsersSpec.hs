module Block3.NotSimpleParsersSpec
  ( spec,
  )
where

import Block3.NotSimpleParsersTask (listlistParser)
import Block3.ParserCombinatorTask (runParser)
import Data.Maybe (isNothing)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
  describe "listlistParser" $ do
    describe "valid" $ do
      it "[], []" $
        runParser listlistParser "0,0" `shouldBe` Just ([[], []], "")
      it "[1]" $
        runParser listlistParser "1,1" `shouldBe` Just ([[1]], "")
      it "[1, 10], [5, -7, 2]" $
        runParser listlistParser "2, 1,+10  , 3,5,-7, 2" `shouldBe` Just ([[1, 10], [5, -7, 2]], "")

    describe "invalid" $ do
      it "extra elems" $
        runParser listlistParser "2,1,+10, 2,5,-7,2" `shouldSatisfy` isNothing
      it "not enough elems" $
        runParser listlistParser "2,1,+10, 4,5,-7,2" `shouldSatisfy` isNothing
      it "negative length" $
        runParser listlistParser "-2,1,10" `shouldSatisfy` isNothing
      it "parse error" $
        runParser listlistParser "2 3 4" `shouldSatisfy` isNothing
      it "parse error" $
        runParser listlistParser "2, -3, 4+" `shouldSatisfy` isNothing
