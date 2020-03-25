module Block1Spec
  ( nextDaySpec
  , afterDaysSpec
  , isWeekendSpec
  , daysToPartySpec
  ) where

import Block1 (Day (..), afterDays, isWeekend, nextDay, daysToParty)
import Test.Hspec (SpecWith, describe, it, shouldBe)

nextDaySpec :: SpecWith ()
nextDaySpec = describe "Block1.nextDay" $ do
  nextDayTest Monday Tuesday
  nextDayTest Tuesday Wednesday
  nextDayTest Wednesday Thursday
  nextDayTest Thursday Friday
  nextDayTest Friday Saturday
  nextDayTest Saturday Sunday
  nextDayTest Sunday Monday
  where
    nextDayTest :: Day -> Day -> SpecWith ()
    nextDayTest today tomorrow =
      it ("should return coorect nextDay for " ++ show today) $ do
        nextDay today `shouldBe` tomorrow

afterDaysSpec :: SpecWith ()
afterDaysSpec = describe "Block1.afterDays" $ do
  afterDaysTest 1 Wednesday Thursday
  afterDaysTest 5 Tuesday Sunday
  afterDaysTest 10 Friday Monday
  afterDaysTest 21 Sunday Sunday
  afterDaysTest 49 Saturday Saturday
  where
    afterDaysTest :: Int -> Day -> Day -> SpecWith ()
    afterDaysTest days curDay result =
      it ("should return correct result for "
          ++ show curDay ++ ", " ++ show days) $ do
              afterDays days curDay `shouldBe` result

isWeekendSpec :: SpecWith ()
isWeekendSpec = describe "Block1.isWeekend" $ do
  notWeekendTest Monday
  notWeekendTest Tuesday
  notWeekendTest Wednesday
  notWeekendTest Thursday
  notWeekendTest Friday
  weekendTest Saturday
  weekendTest Sunday
  where
    notWeekendTest :: Day -> SpecWith ()
    notWeekendTest day =
      it ("should say that " ++ show day ++ " is workday") $ do
        isWeekend day `shouldBe` False
    weekendTest :: Day -> SpecWith ()
    weekendTest day =
      it ("should say that " ++ show day ++ " is weekend") $ do
        isWeekend day `shouldBe` True

daysToPartySpec :: SpecWith ()
daysToPartySpec = describe "Block1.daysToParty" $ do
  daysToPartyTest Monday 4
  daysToPartyTest Tuesday 3
  daysToPartyTest Wednesday 2
  daysToPartyTest Thursday 1
  daysToPartyTest Friday 0
  daysToPartyTest Saturday 6
  daysToPartyTest Sunday 5
  where
    daysToPartyTest :: Day -> Int -> SpecWith ()
    daysToPartyTest day res =
      it ("should calculate correct result for " ++ show day) $ do
        daysToParty day `shouldBe` res
