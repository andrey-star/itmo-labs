module Block1.DayOfWeekSpec
  ( spec,
  ) where

import Block1.DayOfWeekTask
  ( DayOfWeek (..),
    afterDays,
    daysToParty,
    isWeekend,
    nextDay,
  )
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )

spec :: Spec
spec = do
  describe "nextDay" $ do
    it "should be Tuesday" $
      nextDay Monday `shouldBe` Tuesday
    it "should be Tuesday" $
      nextDay Tuesday `shouldBe` Wednesday
    it "should be Tuesday" $
      nextDay Wednesday `shouldBe` Thursday
    it "should be Tuesday" $
      nextDay Thursday `shouldBe` Friday
    it "should be Tuesday" $
      nextDay Friday `shouldBe` Saturday
    it "should be Tuesday" $
      nextDay Saturday `shouldBe` Sunday
    it "should be Tuesday" $
      nextDay Sunday `shouldBe` Monday

  describe "afterDays" $ do
    it "1 should be next day" $
      afterDays Friday 1 `shouldBe` Saturday
    it "1 should be next day" $
      afterDays Saturday 1 `shouldBe` Sunday
    it "7 should be same day" $
      afterDays Monday 7 `shouldBe` Monday
    it "0 should be same day" $
      afterDays Tuesday 0 `shouldBe` Tuesday

  describe "isWeekend" $ do
    it "should be Tuesday" $
      isWeekend Monday `shouldBe` False
    it "should be Tuesday" $
      isWeekend Tuesday `shouldBe` False
    it "should be Tuesday" $
      isWeekend Wednesday `shouldBe` False
    it "should be Tuesday" $
      isWeekend Thursday `shouldBe` False
    it "should be Tuesday" $
      isWeekend Friday `shouldBe` False
    it "should be Tuesday" $
      isWeekend Saturday `shouldBe` True
    it "should be Tuesday" $
      isWeekend Sunday `shouldBe` True

  describe "daysToParty" $ do
    it "should be Tuesday" $
      daysToParty Monday `shouldBe` 4
    it "should be Tuesday" $
      daysToParty Tuesday `shouldBe` 3
    it "should be Tuesday" $
      daysToParty Wednesday `shouldBe` 2
    it "should be Tuesday" $
      daysToParty Thursday `shouldBe` 1
    it "should be Tuesday" $
      daysToParty Friday `shouldBe` 0
    it "should be Tuesday" $
      daysToParty Saturday `shouldBe` 6
    it "should be Tuesday" $
      daysToParty Sunday `shouldBe` 5
