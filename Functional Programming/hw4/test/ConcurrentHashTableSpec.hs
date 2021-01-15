module ConcurrentHashTableSpec
  ( spec,
  )
where

import ConcurrentHashTableTask (ConcurrentHashTable, getCHT, newCHT, putCHT, sizeCHT)
import Control.Concurrent.Async (replicateConcurrently_)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "cht" $ do
    describe "newCHT" $ do
      it "size should be 0" $ do
        t <- newCHT :: IO (ConcurrentHashTable Int Int)
        size <- sizeCHT t
        size `shouldBe` 0
      it "shouldn't have elems" $ do
        t <- newCHT :: IO (ConcurrentHashTable Int Int)
        one <- getCHT 1 t
        one `shouldBe` Nothing
    describe "putCHT + getCHT" $ do
      it "should put new element" $ do
        t <- newCHT :: IO (ConcurrentHashTable Int String)
        putCHT 1 "1" t
        size <- sizeCHT t
        size `shouldBe` 1
        element <- getCHT 1 t
        element `shouldBe` Just "1"
      it "should put new elements" $ do
        t <- newCHT :: IO (ConcurrentHashTable Int String)
        putCHT 1 "1" t
        putCHT 2 "2" t
        putCHT 3 "3" t
        putCHT 4 "4" t
        size <- sizeCHT t
        size `shouldBe` 4
        element1 <- getCHT 1 t
        element1 `shouldBe` Just "1"
        element4 <- getCHT 4 t
        element4 `shouldBe` Just "4"
      it "should update existing element" $ do
        t <- newCHT :: IO (ConcurrentHashTable Int String)
        putCHT 1 "1" t
        putCHT 1 "not 1" t
        size <- sizeCHT t
        size `shouldBe` 1
        element <- getCHT 1 t
        element `shouldBe` Just "not 1"

    describe "concurrent" $ do
      it "should be thread-safe" $ do
        t <- newCHT :: IO (ConcurrentHashTable Int String)
        replicateConcurrently_ 8 (mapM_ (`putInt` t) (take 100 [1, 2 ..]))
        size <- sizeCHT t
        size `shouldBe` 100

        where
          putInt :: Int -> ConcurrentHashTable Int String -> IO ()
          putInt v t = putCHT v (show v) t
