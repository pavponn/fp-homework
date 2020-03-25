module Block3Spec where

import Block3 (maybeConcat, eitherConcat)
import Data.Monoid
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

-- nonEmptySemigroupSpec :: SpecWith ()
-- nonEmptySemigroupSpec = describe "Block3.NonEmpty.Semigroup" $ do
