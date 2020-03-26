{-# LANGUAGE OverloadedStrings #-}

module Block2Hedgehog (tests) where

import Block2 (joinWith, splitOn)
import Data.Foldable (toList)
import Hedgehog

import qualified Block1
import qualified Data.List as List
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

tests :: IO Bool
tests = checkParallel $ Group "Block2 properties" [
  ("toList . fromList === sort", propertySort),
  ("joinWith x . splitOn x === id", propertyJoinWithSplitOn)
  ]

genIntList :: Gen [Int]
genIntList =
  let listLength = Range.linear 0 100000
  in  Gen.list listLength Gen.enumBounded

genString :: Gen String
genString =
  let strLength = Range.linear 0 1000
  in  Gen.string strLength Gen.latin1

propertyJoinWithSplitOn :: Property
propertyJoinWithSplitOn = property $
  forAll genString >>= \s ->
    forAll Gen.latin1 >>= \x ->
    (joinWith x (splitOn x s)) === s

propertySort :: Property
propertySort = property $
  forAll genIntList >>= \xs ->
  toList (Block1.fromList xs) === List.sort xs
