module Block3Spec
  ( maybeConcatSpec
  , eitherConcatSpec
  , nonEmptySemigroupSpec
  , thisOrThatSemigroupSpec
  , nameSemigroupMonoidSpec
  ) where

import Block3
import Data.Monoid (All (..), Sum (..))
import Test.Hspec (SpecWith, describe, it, shouldBe)

maybeConcatSpec :: SpecWith ()
maybeConcatSpec = describe "Block3.maybeConcat" $ do
  it "should concat one Just correctly" $ do
    maybeConcat [Just [1 :: Int, 2, 3]] `shouldBe` [1, 2, 3]

  it "should concat list of Just correctly" $ do
    maybeConcat [Just [1 :: Int, 2, 3], Just [4], Just [5, 6]]
      `shouldBe` [1, 2, 3, 4, 5, 6]

  it "should concat one Nothing only correctly" $ do
    ((maybeConcat [Nothing]) :: [Int]) `shouldBe` []

  it "should concat list of Nothing only correctly" $ do
    ((maybeConcat (replicate 15 Nothing)) :: [Int] ) `shouldBe` []

  it "should concat list of Just and Nothinh correcly" $ do
    maybeConcat [Just [1 :: Int, 2, 3], Nothing, Just [4, 5]]
      `shouldBe` [1, 2, 3, 4, 5]

eitherConcatSpec :: SpecWith ()
eitherConcatSpec = describe "Block3.eitherConcat" $ do
  it "should concat one Right correctly" $ do
    ((eitherConcat [Right [1, 2, 3]]) :: (Sum Int, [Int]))
      `shouldBe` (mempty, [1, 2, 3])

  it "should concat one Left correctly" $ do
    ((eitherConcat [Left [1, 2, 3]]) :: ([Int], Sum Int))
      `shouldBe` ([1, 2, 3], mempty)

  it "should concat Left and Right correclty #1" $ do
    eitherConcat
      [
        Left (Sum (3 :: Int)), Right [1 :: Int, 2, 3],
        Left (Sum 5), Right [4, 5], Left (Sum (-1))
      ]
      `shouldBe` (Sum {getSum = 7}, [1, 2, 3, 4, 5])

  it "should concat Left and Right correclty #2" $ do
    eitherConcat
      [
        Left "H", Right (All True), Left "ey,", Right (All False),
        Left " how'r u", Right (All True), Left "?", Right (All True)
      ]
      `shouldBe` ("Hey, how'r u?", All {getAll = False})

nonEmptySemigroupSpec :: SpecWith ()
nonEmptySemigroupSpec = describe "Block3.NonEmpty (Semigroup)" $ do
  it "should perform <> correctly" $ do
    let x = (1 :: Integer)  :| [2, 3, 4, 5]
    let y = 6  :| [7, 8, 9, 10]
    let z = 11 :| [12, 13]
    let l = x <> (y <> z)
    let r = (x <> y) <> z
    let res = (1 :| [2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13])
    l `shouldBe` r
    l `shouldBe` res
    r `shouldBe` res


thisOrThatSemigroupSpec :: SpecWith ()
thisOrThatSemigroupSpec = describe "Block3.NonEmpty (Semigroup)" $ do
  it "should perform <> correctly for This & This & This" $ do
    let x = ((This "first") :: ThisOrThat String (Sum Int))
    let y = This "second"
    let z = This "third"
    x <> (y <> z) `shouldBe` (x <> y) <> z

  it "should perform <> correctly for This & This & That" $ do
    let x = This "first"
    let y = This "second"
    let z = That (2 :: Sum Int)

    x <> (y <> z) `shouldBe` (x <> y) <> z

  it "should perform <> correctly for This & That & That" $ do
    let x = This "first"
    let y = That (1 :: Sum Int)
    let z = That (2 :: Sum Int)
    x <> (y <> z) `shouldBe` (x <> y) <> z

  it "should perform <> correctly for That & That & That" $ do
    let x = (That (1 :: Sum Int)) :: ThisOrThat String (Sum Int)
    let y = That (2 :: Sum Int)
    let z = That (3 :: Sum Int)
    x <> (y <> z) `shouldBe` (x <> y) <> z

  it "should preform <> correctly for This & Both & That" $ do
    let x = Both "first" (2 :: Sum Int)
    let y = This "second"
    let z = That (4 :: Sum Int)
    x <> (y <> z) `shouldBe` (x <> y) <> z

  it "should preform <> correctly for This & Both & That" $ do
    let x = Both "first" (2 :: Sum Int)
    let y = Both "mine"  (3 :: Sum Int)
    let z = That (4 :: Sum Int)
    x <> (y <> z) `shouldBe` (x <> y) <> z

nameSemigroupMonoidSpec :: SpecWith ()
nameSemigroupMonoidSpec = describe "Block3.Name (Semigroup, Monoid)" $ do
  it "should have correct mempty element" $ do
    let name = Name "someName"
    let x = mempty <> name
    let y = name <> mempty
    x `shouldBe` y
    x `shouldBe` name
    y `shouldBe` name

  it "should perform <> correctly" $ do
    let x = Name "first"
    let y = Name "second"
    let z = Name "third"
    x <> (y <> z) `shouldBe` (x <> y) <> z
    x <> (y <> z) `shouldBe` Name "first.second.third"
    (x <> y) <> z `shouldBe` Name "first.second.third"
