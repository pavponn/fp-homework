module Block1
  ( Day (..)
  , nextDay
  , afterDays
  , isWeekend
  , daysToParty
  , Nat (..)
  , Tree (..)
  , isEmpty
  , size
  , lookup
  , contains
  , singleton
  , insert
  , delete
  , fromList
  ) where

import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.Ratio
import Prelude hiding (lookup)

data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show)

instance Enum Day where
  toEnum x =
    case x of
      0 -> Monday
      1 -> Tuesday
      2 -> Wednesday
      3 -> Thursday
      4 -> Friday
      5 -> Saturday
      6 -> Sunday
      _ -> error "Invalid argument"

  fromEnum day =
    case day of
      Monday    -> 0
      Tuesday   -> 1
      Wednesday -> 2
      Thursday  -> 3
      Friday    -> 4
      Saturday  -> 5
      Sunday    -> 6

instance Eq Day where
  fstDay == sndDay = (fromEnum fstDay) == (fromEnum sndDay)

nextDay :: Day -> Day
nextDay Sunday = Monday
nextDay day    = succ day

afterDays :: Int -> Day -> Day
afterDays shift day = toEnum $ ((fromEnum day) + shift) `mod` 7

isWeekend :: Day -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

daysToParty :: Day -> Int
daysToParty Saturday = 6
daysToParty Sunday   = 5
daysToParty curDay   = fromEnum Friday - fromEnum curDay

data Nat
  = Z
  | S Nat
  deriving (Show)

-- |For Nat, as an instance of Num, following
-- methods are defined: addition (1), multiplication (2),
-- subtraction (3), fromInteger (4).
-- Others for minimal complete definition: abs, signum.
instance Num Nat where
  n + Z     = n
  n + (S m) = S (n + m)

  _ * Z     = Z
  n * (S m) = (n * m) + n

  abs = id

  signum Z = 0
  signum _ = 1

  fromInteger x
    | x == 0     = Z
    | x > 0      = S $ fromInteger $ x - 1
    | otherwise  = error "Invalid argument"

  n - Z         = n
  Z - _         = Z
  (S n) - (S m) = n - m

-- |For Nat, as an instance of Eq,
-- equality (5) defined.
instance Eq Nat where
  Z     == Z     = True
  (S x) == (S y) = x == y
  _     == _     = False

-- |For Nat, as an instance of Ord,
-- comparision (6) defined.
instance Ord Nat where
  compare Z Z         = EQ
  compare _ Z         = GT
  compare Z _         = LT
  compare (S x) (S y) = compare x y

-- |Implementation of typeclass `Enum` is required for
-- typeclass `Integral`.
instance Enum Nat where
  toEnum x
    | x == 0     = Z
    | x > 0      = S $ toEnum $ x - 1
    | otherwise  = error "Invalid argument"

  fromEnum n = fromEnumHelp n 0 where
    fromEnumHelp :: Nat -> Int -> Int
    fromEnumHelp Z     acc = acc
    fromEnumHelp (S m) acc = fromEnumHelp m (acc + 1)

-- |Implementation of typeclass `Real` is required for
-- typeclass `Integral`.
instance Real Nat where
  toRational n = (toInteger n) % 1

-- |For Nat, as an instance of Integral, following
-- methods defined: toInteger (4),
-- integer division (8) and remainder of division (9)
-- using `quotRem`. Even function (7) is defined
-- using `rem`, that uses `quotRem`.
instance Integral Nat where
  toInteger n = toIntegerHelp n 0 where
    toIntegerHelp :: Nat -> Integer -> Integer
    toIntegerHelp Z acc     = acc
    toIntegerHelp (S m) acc = toIntegerHelp m (acc + 1)

  quotRem _ Z = error "Division by zero"
  quotRem n m
    | (n < m)   = (0, n)
    | otherwise =
      let (q, r) = quotRem (n - m) m
      in (q + 1, r)

data Tree a
  = Nil
  | Node (NonEmpty a) (Tree a) (Tree a)
  deriving (Show)

-- |Checks whether given Tree is empty.
isEmpty :: Tree a -> Bool
isEmpty Nil = True
isEmpty _   = False

-- |Returns size of the given Tree.
size :: Tree a -> Int
size Nil           = 0
size (Node xs l r) = length xs + size l + size r

-- |Checks whether element is in Tree,
-- returns `Just element` if it exists, otherwise Nothing.
lookup :: (Ord a) => a -> Tree a -> Maybe a
lookup _ Nil = Nothing
lookup e (Node (x :| _) l r) =
  case compare e x of
    LT -> lookup e l
    GT -> lookup e r
    EQ -> Just x

-- |Checks whether element is in Tree,
contains :: (Ord a) => a -> Tree a -> Bool
contains e tree =
  case lookup e tree of
    Nothing -> False
    _       -> True

-- |Creates Tree from one element.
singleton :: (Ord a) => a -> Tree a
singleton e = Node (e :| []) Nil Nil

-- |Inserts element in the tree.
insert :: (Ord a) => a -> Tree a -> Tree a
insert e Nil = singleton e
insert e (Node list@(x :| _) l r) =
  case compare e x of
    LT -> (Node list (insert e l) r)
    GT -> (Node list l (insert e r))
    EQ -> (Node (e <| list) l r)

-- |Deletes element in the tree.
delete :: (Ord a) => a -> Tree a -> Tree a
delete _ Nil = Nil
delete e (Node list@(x :| xs) l r) =
  case compare e x of
    LT -> (Node list (delete e l) r)
    GT -> (Node list l (delete e r))
    EQ -> case xs of
            (xx : xxs) -> (Node (xx :| xxs) l r)
            []         -> unify l r
  where
    unify :: Ord a => Tree a -> Tree a -> Tree a
    unify Nil  right = right
    unify left Nil   = left
    unify (Node lvals ll lr) right = (Node lvals ll (unify lr right))

-- |Creates Tree from list.
fromList :: Ord a => [a] -> Tree a
fromList = foldr insert  Nil

{-
bigTree = fromList [1,2,3,4,5,6,1,1,1,100]
newTree = insert  0  (insert (-1) bigTree)
 without1 = 1 `delete`(1 `delete` (1 `delete` (1 `delete` newTree)))
-}
