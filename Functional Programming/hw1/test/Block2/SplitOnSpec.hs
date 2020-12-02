module Block2.SplitOnSpec (spec) where

import Block2.SplitOnTask (joinWith, splitOn)
import Data.List.NonEmpty (NonEmpty (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "splitOn" $ do
    it "string" $
      splitOn '/' "path/to/file" `shouldBe` "path" :| ["to", "file"]
    it "int" $
      splitOn 3 [1, 2, 3, 4, 3, 3, 5, 6, 3, 7] `shouldBe` [1, 2] :| [[4], [], [5, 6], [7]]
    it "no match" $
      splitOn 3 [1, 2, 4, 5, 6, 7] `shouldBe` [1, 2, 4, 5, 6, 7] :| []
    it "empty" $
      splitOn 3 [] `shouldBe` [] :| []

  describe "joinWith" $ do
    it "string" $
      joinSplit '/' "path/to/file" `shouldBe` "path/to/file"
    it "string" $
      joinSplit 3 [1, 2, 3, 3, 4, 3, 5, 6, 7, 3, 7, 3] `shouldBe` [1, 2, 3, 3, 4, 3, 5, 6, 7, 3, 7, 3]
    it "empty" $
      joinWith 2 ([1] :| []) `shouldBe` [1]
  where
    joinSplit x = joinWith x . splitOn x
