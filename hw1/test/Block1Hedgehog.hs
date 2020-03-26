{-# LANGUAGE OverloadedStrings #-}

module Block1Hedgehog (tests) where

import Block1 (Nat (..))
import Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

tests :: IO Bool
tests = checkParallel $ Group "Block1 properties" [
  ("Nat: toInteger . fromInteger === id", propertyNatFromIntegerToInteger),
  ("Z * x === Z", propertyZmultiply1),
  ("x * Z === Z", propertyZmultiply2),
  ("Z + x === x", propertyZsum1),
  ("x + Z === x", propertyZsum2)
  ]

propertyNatFromIntegerToInteger :: Property
propertyNatFromIntegerToInteger = property $
  forAll genInteger >>= \x ->
  toInteger ((fromInteger x) :: Nat) === x

propertyZmultiply1 :: Property
propertyZmultiply1 = property $
  forAll genInteger >>= \x ->
  Z * (fromInteger x) === Z

propertyZmultiply2 :: Property
propertyZmultiply2 = property $
  forAll genInteger >>= \x ->
  (fromInteger x) * Z === Z

propertyZsum1 :: Property
propertyZsum1 = property $
  forAll genInteger >>= \x ->
  (fromInteger x) + Z === (fromInteger x)

propertyZsum2 :: Property
propertyZsum2 = property $
  forAll genInteger >>= \x ->
  Z + (fromInteger x) === (fromInteger x)

genInteger :: Gen Integer
genInteger = Gen.integral (Range.linear 0 100)
