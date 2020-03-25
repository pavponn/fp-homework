module Block2Spec
  ( splitOnSpec
  , joinWithSpec
  ) where

import Block2 (joinWith, splitOn)
import Data.List.NonEmpty (NonEmpty (..))
import Test.Hspec (SpecWith, describe, it, shouldBe)

splitOnSpec :: SpecWith ()
splitOnSpec = describe "Block2.splitOn" $ do
  it "should split on non occurring element correctly" $ do
    splitOn '/' "string" `shouldBe` ("string" :| [])

  it "should split on once occurring element correctly" $ do
    splitOn '/' "one/two" `shouldBe` ("one" :| ["two"])

  it "should split on several times occurring element correctly" $ do
    splitOn 'a' "goaletsagoayes" `shouldBe` ("go" :| ["lets", "go", "yes"])

joinWithSpec :: SpecWith ()
joinWithSpec = describe "Block2.joinWith" $ do
  it "should join nonemty of one element" $ do
    joinWith 'a' ("str" :| []) `shouldBe` "str"

  it "should join correctly #1" $ do
    joinWith '/' ("path" :| ["to", "file"]) `shouldBe` "path/to/file"

  it "should join correctly #2" $ do
    joinWith ' ' ("path" :| ["to", "file", "nope"]) `shouldBe` "path to file nope"
