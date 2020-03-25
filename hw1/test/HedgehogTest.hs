module Main where

import qualified Block2Hedgehog (tests)

main :: IO ()
main = do
  Block2Hedgehog.tests
  return ()
