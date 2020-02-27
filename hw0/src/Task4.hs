module Task4
  ( iterateElement
  , fibonacci
  , factorial
  , mapFix
  ) where

import Data.Function (fix)

-- |This function creates infinite list out of given argument.
iterateElement :: a -> [a]
iterateElement = fix (\f x -> x : (f x))

-- |This function returns n'th number of Fibonacci.
fibonacci :: Integer -> Integer
fibonacci = fix $ \f n ->
  if (n < 0)
    then ((-1) ^ (abs n + 1)) * (f (abs n))
  else if n == 0
    then 0
  else if n <= 2
    then 1
  else (f (n - 1)) + (f (n - 2))

-- |This functions return factorial of a given number.
-- If provided argument is less then 0 returns undefined.
factorial :: Integer -> Integer
factorial = fix $ \f n ->
  if n < 0
    then undefined
  else if (n == 0)
    then 1
  else n * f (n - 1)

-- |The 'mapFix' function applies function to the elements of list.
mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix helpFunction
  where
    helpFunction :: ((a -> b) -> [a] -> [b]) -> (a -> b) -> [a] -> [b]
    helpFunction = \f mapFun list ->
      case list of
        []       -> []
        (x : xs) -> mapFun x : (f mapFun xs)
