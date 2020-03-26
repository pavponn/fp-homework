module Block6Spec where

import Block6
import Control.Applicative
import Test.Hspec (SpecWith, describe, it, shouldBe)

parserSpec :: SpecWith ()
parserSpec = describe "Block6.Parser" $ do
  it "should be correct functor" $ do
    runParser ((*2) <$> element (5 :: Integer)) [5, 1, 2] `shouldBe` Just (10, [1, 2])
  it "should be correct applicative" $ do
    runParser ((+) <$> satisfy (> 2) <*> satisfy (> 3)) [3 :: Integer, 4]
      `shouldBe` Just (7, [])
  it "should be correct alternative" $ do
    runParser (satisfy (>2) <|> satisfy (==(-1))) [-1 :: Integer, 4]
      `shouldBe` Just (-1, [4])
  it "should be correct monad"  $ do
    runParser (element 'a' >>= \a  -> element 'b' >>= \b -> return [a,'+', b]) "abc"
      `shouldBe` Just ("a+b", "c")

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
    runParser (satisfy (> 2)) [5 :: Integer, 1] `shouldBe` Just (5, [1])
  it "should reject element that doesn't satisfy predicate" $ do
    runParser (satisfy (< 2)) [5 :: Integer, 1] `shouldBe` Nothing

elementSpec :: SpecWith ()
elementSpec = describe "Block6.element" $ do
  it "should parse correct element" $ do
    runParser (element 5) [5 :: Integer, 1, 2] `shouldBe` Just (5, [1,2])
  it "should fail on empty input" $ do
    runParser (element '0') [] `shouldBe` Nothing
  it "should fail on incorrect element" $ do
    runParser (element 's') "aaa" `shouldBe` Nothing

streamSpec :: SpecWith ()
streamSpec = describe "Block6.stream" $ do
  it "should parse correct stream" $ do
    runParser (stream [5, 1]) [5 :: Integer, 1, 2] `shouldBe` Just ([5,1], [2])
  it "should fail on empty input" $ do
    runParser (stream [2 :: Integer, 22]) [] `shouldBe` Nothing
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

listlistParserSpec :: SpecWith ()
listlistParserSpec = describe "Block6.listlistParser" $ do
  it "should accept and parse correclty" $ do
    runParser listlistParser "0" `shouldBe` Just ([[]], "")
    runParser listlistParser "1, 1" `shouldBe` Just ([[1]], "")
    runParser listlistParser "2, 1,+10, 3,5,-7, 2"
      `shouldBe` Just ([[1, 10], [5, -7, 2]], "")
    runParser listlistParser "2, 1,+102, 1, 1, 0, 1, 1, 2, 1, -3"
      `shouldBe` Just ([[1,102],[1],[],[1],[1,-3]],"")
  it "should reject incorrect input" $ do
    runParser listlistParser "1 2, 3" `shouldBe` Nothing
    runParser listlistParser "1, -40, 3, 1, 2" `shouldBe` Nothing
    runParser listlistParser "1, 2 aa" `shouldBe` Nothing
