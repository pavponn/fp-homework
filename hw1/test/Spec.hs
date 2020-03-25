import Test.Hspec (hspec, SpecWith)

import Block3Spec (maybeConcatSpec, eitherConcatSpec)
import Block4Spec (stringSumSpec)
import Block5Spec (evalSpec, movingSpec)
import Block6Spec (parserSpec, okSpec, eofSpec, satisfySpec, elementSpec,
                   streamSpec, cbsParserSpec, intParserSpec, listlistParserSpec)

main :: IO ()
main =
  hspec $ do
    block3Tests
    block4Tests
    block5Tests
    block6Tests

block3Tests :: SpecWith ()
block3Tests = do
  maybeConcatSpec
  eitherConcatSpec

block4Tests :: SpecWith ()
block4Tests = do
  stringSumSpec

block5Tests :: SpecWith ()
block5Tests = do
  evalSpec
  movingSpec

block6Tests :: SpecWith ()
block6Tests = do
  parserSpec
  okSpec
  eofSpec
  satisfySpec
  elementSpec
  streamSpec
  cbsParserSpec
  intParserSpec
  listlistParserSpec
