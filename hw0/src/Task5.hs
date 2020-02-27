module Task5
    ( zero
    , succChurch
    , churchPlus
    , churchMult
    , churchToInt
    , churchFromInt
    ) where

type Nat a = (a -> a) -> a -> a

-- |This function returns zero church numeral.
zero :: Nat a
zero _ x = x

-- |This function increments church numeral by one.
succChurch :: Nat a -> Nat a
succChurch = \n f x -> f (n f x)

-- |This function add up two church numerals.
churchPlus :: Nat a -> Nat a -> Nat a
churchPlus = \n m f x -> n f (m f x)

-- |This function multiplies two church numerals.
churchMult :: Nat a -> Nat a -> Nat a
churchMult = \n m f x -> n (m f) x

-- |This function transforms church numeral to Integer.
churchToInt :: Nat Integer -> Integer
churchToInt n = n (+1) 0

-- |This function transforms Integer to church numeral.
churchFromInt :: Integer -> Nat Integer
churchFromInt n
  | n < 0     = error "Non-negative integer expected"
  | n == 0    = zero
  | otherwise = succChurch $ churchFromInt (n - 1)
