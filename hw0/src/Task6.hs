module Task6
    ( firstExp
    , secondExp
    ) where

import Data.Maybe (mapMaybe)
import Task1 (distributivity)

-- |First expression of Task 6.
firstExp :: (Either [Char] a, Either [Char] b)
firstExp = distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))
-- WHMF of firstExp:
-- ((Left ("harold" ++ " hide " ++ "the " ++ "pain")), (Left ("harold" ++ " hide " ++ "the " ++ "pain")))

-- |Second expression of Task 6.
secondExp :: Bool
secondExp = null $ mapMaybe foo "pole chudes ochen' chudesno"
-- WHNF of secondExp:
-- False

foo :: Char -> Maybe Double
foo char =
  case char == 'o' of
    True  -> Just $ exp pi
    False -> Nothing
