module Main where

import qualified Block1Hedgehog (tests)
import qualified Block2Hedgehog (tests)

main :: IO ()
main = do
  _ <- Block1Hedgehog.tests
  _ <- Block2Hedgehog.tests
  return ()
