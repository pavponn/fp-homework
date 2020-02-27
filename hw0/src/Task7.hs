{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Task7
  ( firstExp
  , secondExp
  , thirdExp
  ) where

import Data.Either (lefts, rights)

-- |First expression of Task 7.
-- null . head $ map (uncurry id) [((++) "Dorian ", " Grey")]
firstExp :: Bool
firstExp = e18
  where
    e1  = " Grey" :: [Char]
    e2  = "Dorian " :: [Char]
    e3  = (++) :: [Char] -> [Char] -> [Char]
    e4  = e3 e2 :: [Char] -> [Char]
    e5  = (e4, e1) :: ([Char] -> [Char], [Char])
    e6  = [e5] :: [([Char] -> [Char], [Char])]
    e7  = uncurry :: (([Char] -> [Char]) -> [Char] -> [Char]) ->
      ([Char] -> [Char], [Char]) -> [Char]
    e8  = id :: ([Char] -> [Char]) -> ([Char] -> [Char])
    e9  = e7 e8 :: ([Char] -> [Char], [Char]) -> [Char]
    e10 = map :: (([Char] -> [Char], [Char]) -> [Char]) ->
      [([Char] -> [Char], [Char])] -> [[Char]]
    e11 = e10 e9 :: [([Char] -> [Char], [Char])] -> [[Char]]
    e12 = e11 e6 :: [[Char]]
    e13 = null :: [Char] -> Bool
    e14 = (.) :: ([Char] -> Bool) -> ([[Char]] -> [Char]) -> [[Char]] -> Bool
    e15 = head :: [[Char]] -> [Char]
    e16 = ($) :: ([[Char]] -> Bool) -> [[Char]] -> Bool
    e17 = e13 `e14` e15 :: [[Char]] -> Bool
    e18 = e17 `e16` e12 :: Bool

-- |Second expression of Task 7.
-- (\x -> zip (lefts x) (rights x)) [Left (1 + 2), Right (2 ^ 6)]
secondExp :: (Num a, Num b) => [(a, b)]
secondExp = secondLeft secondRight
  where
    -- [Left (1 + 2), Right (2 ^ 6)]
    secondRight :: (Num a, Num b) => [Either a b]
    secondRight = e13
      where
        e1  = (^) :: (Integral b, Num a) => a -> b -> a
        e2  = 2 :: Num p => p
        e3  = 6 :: Integer
        e4  = e1 e2 ::  (Integral b, Num a) => b -> a
        e5  = e4 e3 :: Num a => a
        e6  = (+) :: Num a => a -> a -> a
        e7  = 1 :: Num p => p
        e8  = 2 :: Num p => p
        e9  = e6 e7 :: Num a => a -> a
        e10 = e9 e8 :: Num a => a
        e11 = Right e5 :: Num b => Either a b
        e12 = Left e10 ::  Num a => Either a b
        e13 = [e12, e11] :: (Num a, Num b) => [Either a b]
    -- (\x -> zip (lefts x) (rights x))
    secondLeft :: forall p q . (Num p, Num q) => [Either p q] -> [(p, q)]
    secondLeft = \(x :: [Either p q]) ->
      ((e3 ((e2 x) :: [p])) :: [q] -> [(p, q)]) (e1 x :: [q]) :: [(p, q)]
      where
        e1 = rights :: [Either p q] -> [q]
        e2 = lefts :: [Either p q] -> [p]
        e3 = zip :: [p] -> [q] -> [(p, q)]

-- |Third expression of Task 7.
-- let impl = \x y -> not x || y in
--     let isMod2 = \x -> x `mod` 2 == 0 in
--     let isMod4 = \x -> x `mod` 4 == 0 in
--     \x -> (isMod4 x) `impl` (isMod2 x)
thirdExp :: forall a. Integral a => a -> Bool
thirdExp = let
             impl :: Bool -> Bool -> Bool
             impl = \(x :: Bool) (y :: Bool) ->
               ((e1 (e2 (x :: Bool) :: Bool) :: Bool -> Bool) (y :: Bool) :: Bool)
           in
             let
               isMod2 :: Integral a => a -> Bool
               isMod2 = \(x :: a) ->
                 (e4 ((e3 (x :: Integral a => a) :: Integral a => a -> a) t2
                   :: Integral a => a) :: Integral a => a -> Bool) t0 :: Bool
             in
               let
                 isMod4 :: Integral a => a -> Bool
                 isMod4 = \(x :: a) ->
                   (e4 ((e3 (x :: Integral a => a) :: Integral a => a -> a) t4
                     :: Integral a => a) :: Integral a => a -> Bool) t0 :: Bool
               in
                 \(x :: a) ->
                   (impl (isMod4 (x :: Integral a => a) :: Bool) :: Bool -> Bool)
                     (isMod2 (x :: Integral a => a) :: Bool) :: Bool
  where
    e1 = (||) :: Bool -> Bool -> Bool
    e2 = not :: Bool -> Bool
    e3 = (mod) :: Integral a => a -> a -> a
    e4 = (==) :: Eq a => a -> a -> Bool
    t0 = 0 :: Integral a => a
    t2 = 2 :: Integral a => a
    t4 = 4 :: Integral a => a
