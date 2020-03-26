module Block1Spec
  ( nextDaySpec
  , afterDaysSpec
  , isWeekendSpec
  , daysToPartySpec
  , natSpec
  , treeSpec
  ) where

import Block1 (Day (..), Nat (..), Tree (..), afterDays, contains, daysToParty, delete, fromList,
               insert, isEmpty, isWeekend, nextDay, singleton, size)
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

natSpec :: SpecWith ()
natSpec = describe "Block1.Nat" $ do
  it "should sum Nats correctly" $ do
    two + seven `shouldBe` nine
    three + four `shouldBe` seven
    Z + seven `shouldBe` seven
    seven + Z `shouldBe` seven

  it "should multiply Nats correctly" $ do
    two * two `shouldBe` four
    three * two `shouldBe` six
    three * Z `shouldBe` Z
    Z * three `shouldBe` Z

  it "should subtract Nats correctly" $ do
    four - two `shouldBe` two
    seven - four `shouldBe` three
    seven - Z `shouldBe` seven
    Z - seven `shouldBe` Z

  it "should convert Nats to Integer correctly" $ do
    toInteger two `shouldBe` 2
    toInteger three `shouldBe` 3
    toInteger Z `shouldBe` 0
    toInteger nine `shouldBe` 9

  it "should convert Nats to Integer correctly" $ do
    fromInteger 2 `shouldBe` two
    fromInteger 3 `shouldBe` three
    fromInteger 4 `shouldBe` four
    fromInteger 6 `shouldBe` six
    fromInteger 0 `shouldBe` Z
    fromInteger 9 `shouldBe` nine

  it "should compare Nats correctly" $ do
    compare two two `shouldBe` EQ
    compare two four `shouldBe` LT
    compare nine three `shouldBe` GT

  it "should check whether Nat is even correclty" $ do
    even two `shouldBe` True
    even six `shouldBe` True
    even nine `shouldBe` False
    even seven `shouldBe` False
    even Z `shouldBe` True

  it "should calculate quot for Nats correctly" $ do
    quot four two `shouldBe` two
    quot six four `shouldBe` one
    quot seven two `shouldBe` three
    quot one three `shouldBe` Z

  it "should calculate rem for Nats correctly" $ do
    rem four two `shouldBe` Z
    rem six four `shouldBe` two
    rem seven two `shouldBe` one
    rem one three `shouldBe` one

  where
    one :: Nat
    one = S Z
    two :: Nat
    two = S $ S Z
    three :: Nat
    three = S $ S $ S Z
    four :: Nat
    four = S $ S $ S $ S Z
    six :: Nat
    six = S $ S $ S $ S $ S $ S Z
    seven :: Nat
    seven = S $ S $ S $ S $ S $ S $ S Z
    nine :: Nat
    nine = S $ S $ S $ S $ S $ S $ S $ S $ S Z

treeSpec :: SpecWith ()
treeSpec = describe "Block1.Tree" $ do
  it "isEmpty should return true if tree is empty" $ do
    isEmpty Nil `shouldBe` True

  it "isEmpty should return false if tree is not empty" $ do
    isEmpty (singleton (5 :: Int)) `shouldBe` False

  it "size should calculate size correctly" $ do
    size (fromList [4 :: Int, 2, 3, 3]) `shouldBe` 4

  it "contains should return false if there is no element in the tree" $ do
    contains 5 (fromList [1 :: Int, 4, 2, 3]) `shouldBe` False

  it "contains should return true if there is element in the tree" $ do
    contains 5 (fromList [1:: Int, 4, 2, 3, 5]) `shouldBe` True

  it "should insert new element correctly" $ do
    let tree = fromList [1 :: Int, 2, 1, 3, 4]
    let newTree = insert 0 tree
    contains 0 newTree `shouldBe` True

  it "should insert same element correctly" $ do
    let bigTree = fromList [1 :: Int, 2, 3, 4, 5, 6, 1, 1, 1, 100]
    let newTree = insert 0 bigTree
    contains 0 newTree `shouldBe` True
    size newTree `shouldBe` 11

  it "should delete sinlge element correctly" $ do
    let bigTree = fromList [1 :: Int, 2, 3, 4, 5, 6, 1, 1, 1, 100]
    let newTree = delete 5 bigTree
    contains 5 newTree `shouldBe` False

  it "should delete multiple element correctly" $ do
    let bigTree = fromList [1 :: Int, 2, 3, 4, 5, 6, 1, 1, 1, 100]
    let newTree = delete 1 bigTree
    contains 1 newTree `shouldBe` True
    size newTree `shouldBe` 9
