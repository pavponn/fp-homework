import Test.Hspec (hspec)

import Block6Spec (okSpec, eofSpec, satisfySpec, elementSpec,
                   streamSpec, cbsParserSpec, intParserSpec )

main :: IO ()
main =
  hspec $ do
    okSpec
    eofSpec
    satisfySpec
    elementSpec
    streamSpec
    cbsParserSpec
    intParserSpec

