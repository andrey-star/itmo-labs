module Block1.TreeSpec (spec) where

import Block1.TreeTask (Tree (..), add, contains, empty, fromList, remove, size)
import Data.List.NonEmpty (NonEmpty (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "fromList" $ do
    it "should construct tree" $
      fromList [1, 2, 3, 4, 5] `shouldBe` oneToFive
    it "should construct tree" $
      fromList [5, 4, 3, 2, 1] `shouldBe` fiveToOne
    it "should construct tree" $
      fromList [3, 3, 1, 1, 2, 2] `shouldBe` threeOneTwoTwice

  describe "empty" $ do
    it "Leaf should be empty" $
      empty Leaf `shouldBe` True
    it "not Leaf shouldn't be empty" $
      empty (fromList [1]) `shouldBe` False

  describe "size" $ do
    it "empty size 0" $
      size Leaf `shouldBe` 0
    it "5 els size 5" $
      size oneToFive `shouldBe` 5
    it "should count reps" $
      size threeOneTwoTwice `shouldBe` 6

  describe "contains" $ do
    it "empty doesnt contain" $ do
      contains Leaf 0 `shouldBe` False
    it "empty doesnt contain" $ do
      contains Leaf 1 `shouldBe` False
    it "oneToFive" $ do
      contains oneToFive 0 `shouldBe` False
    it "oneToFive" $ do
      contains oneToFive 1 `shouldBe` True
    it "oneToFive" $ do
      contains oneToFive 2 `shouldBe` True
    it "oneToFive" $ do
      contains oneToFive 3 `shouldBe` True
    it "oneToFive" $ do
      contains oneToFive 4 `shouldBe` True
    it "oneToFive" $ do
      contains oneToFive 5 `shouldBe` True
    it "oneToFive" $ do
      contains oneToFive 6 `shouldBe` False

  describe "add" $ do
    it "should add to empty" $ do
      add Leaf 1 `shouldBe` one
    it "should add rep" $
      add one 1 `shouldBe` Node (1 :| [1]) Leaf Leaf
    it "should add rep" $
      add (Node (1 :| [1]) Leaf Leaf) 1 `shouldBe` Node (1 :| [1, 1]) Leaf Leaf
    it "should add rep" $
      add threeOneTwoTwice 1 `shouldBe` Node (3 :| [3]) (Node (1 :| [1, 1]) Leaf (Node (2 :| [2]) Leaf Leaf)) Leaf

  describe "remove" $ do
    it "remove form empty" $
      remove Leaf 1 `shouldBe` Leaf
    it "remove with reps" $
      remove threeOneTwoTwice 1 `shouldBe` Node (3 :| [3]) (Node (1 :| []) Leaf (Node (2 :| [2]) Leaf Leaf)) Leaf
    it "remove last occ" $
      remove oneToFive 3 `shouldBe` Node (1 :| []) Leaf (Node (2 :| []) Leaf (Node (4 :| []) Leaf (Node (5 :| []) Leaf Leaf)))
    it "remove last from tree" $
       remove (remove oneToSeven 6) 7 `shouldBe` Node (4 :| []) 
         (Node (2 :| []) (Node (1 :| []) Leaf Leaf) (Node (3 :| []) Leaf Leaf)) 
         (Node (5 :| []) Leaf Leaf)
  where
    one = Node (1 :| []) Leaf Leaf
    fiveToOne = Node (5 :| []) (Node (4 :| []) (Node (3 :| []) (Node (2 :| []) (Node (1 :| []) Leaf Leaf) Leaf) Leaf) Leaf) Leaf
    oneToFive = Node (1 :| []) Leaf (Node (2 :| []) Leaf (Node (3 :| []) Leaf (Node (4 :| []) Leaf (Node (5 :| []) Leaf Leaf))))
    threeOneTwoTwice = Node (3 :| [3]) (Node (1 :| [1]) Leaf (Node (2 :| [2]) Leaf Leaf)) Leaf
    oneToSeven = fromList [4, 2, 6, 1, 3, 5, 7]
