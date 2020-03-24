module Block6Spec where

import Block6

import Test.Hspec (Spec, SpecWith, describe, it, shouldBe)

okSpec :: SpecWith ()
okSpec = describe "Block6.ok" $ do
  it "ok #1" $ do
    runParser ok "aaa" `shouldBe` Just ((), "aaa")
  it "ok #2" $ do
    runParser ok "" `shouldBe` Just ((), "")

eofSpec :: SpecWith ()
eofSpec = describe "Block6.eof" $ do
  it "should accept empty input" $ do
    runParser eof "" `shouldBe` Just ((), "")
  it "should reject non-empty input" $ do
    runParser eof "aa" `shouldBe` Nothing

satisfySpec :: SpecWith ()
satisfySpec = describe "Block6.satisfy" $ do
  it "should accept element that satisfies predicate" $ do
    runParser (satisfy (> 2)) [5,1] `shouldBe` Just (5, [1])
  it "should reject element that doesn't satisfy predicate" $ do
    runParser (satisfy (< 2)) [5,1] `shouldBe` Nothing

elementSpec :: SpecWith ()
elementSpec = describe "Block6.element" $ do
  it "should parse correct element" $ do
    runParser (element 5) [5,1,2] `shouldBe` Just (5, [1,2])
  it "should fail on empty input" $ do
    runParser (element '0') [] `shouldBe` Nothing
  it "should fail on incorrect element" $ do
    runParser (element 's') "aaa" `shouldBe` Nothing

streamSpec :: SpecWith ()
streamSpec = describe "Block6.stream" $ do
  it "should parse correct stream" $ do
    runParser (stream [5,1]) [5,1,2] `shouldBe` Just ([5,1], [2])
  it "should fail on empty input" $ do
    runParser (stream [2, 22]) [] `shouldBe` Nothing
  it "should fail on incorrect stream" $ do
    runParser (stream "aab") "aaab" `shouldBe` Nothing

cbsParserSpec :: SpecWith ()
cbsParserSpec = describe "Block6.parseCBS" $ do
  it "should accept correct bracket sequence" $ do
    runParser cbsParser "" `shouldBe` Just ((), "")
    runParser cbsParser "()" `shouldBe` Just ((), "")
    runParser cbsParser "()()" `shouldBe` Just ((), "")
    runParser cbsParser "(())" `shouldBe` Just ((), "")
    runParser cbsParser "(()())" `shouldBe` Just ((), "")
    runParser cbsParser "(())()()(()())" `shouldBe` Just ((), "")
  it "should reject incorrect bracket sequence" $ do
    runParser cbsParser "(" `shouldBe` Nothing
    runParser cbsParser "(()))" `shouldBe` Nothing
    runParser cbsParser "(()())(())(" `shouldBe` Nothing

intParserSpec :: SpecWith ()
intParserSpec = describe "Block6.intParser" $ do
  it "shoud accept integer" $ do
    runParser intParser "5" `shouldBe` Just (5, "")
    runParser intParser "+123" `shouldBe` Just (123, "")
    runParser intParser "-123" `shouldBe` Just ((-123), "")
  it "shoud reject integer" $ do
      runParser intParser "--5" `shouldBe` Nothing
      runParser intParser "aaa" `shouldBe` Nothing
      runParser intParser "+ 25 a" `shouldBe` Nothing

