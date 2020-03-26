import Test.Hspec (SpecWith, hspec)

import Block1Spec (afterDaysSpec, daysToPartySpec, isWeekendSpec, natSpec, nextDaySpec, treeSpec)
import Block2Spec (joinWithSpec, splitOnSpec)
import Block3Spec (eitherConcatSpec, maybeConcatSpec, nameSemigroupMonoidSpec,
                   nonEmptySemigroupSpec, thisOrThatSemigroupSpec)
import Block4Spec (stringSumSpec)
import Block5Spec (evalSpec, movingSpec)
import Block6Spec (cbsParserSpec, elementSpec, eofSpec, intParserSpec, listlistParserSpec, okSpec,
                   parserSpec, satisfySpec, streamSpec)

main :: IO ()
main =
  hspec $ do
    block1Tests
    block2Tests
    block3Tests
    block4Tests
    block5Tests
    block6Tests

block1Tests :: SpecWith ()
block1Tests = do
  nextDaySpec
  afterDaysSpec
  isWeekendSpec
  daysToPartySpec
  natSpec
  treeSpec

block2Tests :: SpecWith ()
block2Tests = do
  splitOnSpec
  joinWithSpec

block3Tests :: SpecWith ()
block3Tests = do
  maybeConcatSpec
  eitherConcatSpec
  nonEmptySemigroupSpec
  thisOrThatSemigroupSpec
  nameSemigroupMonoidSpec

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
