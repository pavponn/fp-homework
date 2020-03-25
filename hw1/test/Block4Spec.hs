module Block4Spec where

import Block4 (stringSum)
import Test.Hspec (SpecWith, describe, it, shouldBe)

stringSumSpec :: SpecWith ()
stringSumSpec = describe "Block4.stringSum" $ do
  it "should calculate sum from 1 number correctly" $ do
    stringSum "1" `shouldBe` Just 1

  it "should calculate sum from 2 numbers correcly" $ do
    stringSum "5 3" `shouldBe` Just 8

  it "should calculate sum from multiple numbers correcly" $ do
    stringSum "10 20 3 4 -5 2 11" `shouldBe` Just 45

  it "should return Nothing, if input is invalid" $ do
    stringSum "4 5  a 6 4" `shouldBe` Nothing
