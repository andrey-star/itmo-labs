module Block2.FoldableTreeSpec (spec) where

import Block2.FoldableTreeTask (Tree (..), fromList)
import Data.List.NonEmpty (NonEmpty (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "foldr" $ do
    it "sum 1..5 = 15" $
      foldr (+) 0 oneToFive `shouldBe` 15
    it "sum 5..1 = 15" $
      foldr (+) 0 fiveToOne `shouldBe` 15

  describe "foldMap" $ do
    it "1..7 BST maps to 1..7" $
      foldMap (: []) oneToSeven `shouldBe` [1, 2, 3, 4, 5, 6, 7]
  where
    fiveToOne = Node (5 :| []) (Node (4 :| []) (Node (3 :| []) (Node (2 :| []) (Node (1 :| []) Leaf Leaf) Leaf) Leaf) Leaf) Leaf
    oneToFive = Node (1 :| []) Leaf (Node (2 :| []) Leaf (Node (3 :| []) Leaf (Node (4 :| []) Leaf (Node (5 :| []) Leaf Leaf))))
    oneToSeven = fromList [4, 2, 6, 1, 3, 5, 7]
