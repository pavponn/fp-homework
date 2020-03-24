module Block1 where

import Data.Ratio
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.List(sort)
import Prelude hiding (lookup)

------------
-- Task 1 --
------------
data WeekDay
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show)

instance Enum WeekDay where
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

nextDay :: WeekDay -> WeekDay
nextDay Sunday = Monday
nextDay day    = succ day

afterDays :: Int -> WeekDay -> WeekDay
afterDays shift day = toEnum $ ((fromEnum day) + shift) `mod` 7

isWeekend :: WeekDay -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

daysToParty :: WeekDay -> Int
daysToParty Saturday = 6
daysToParty Sunday   = 5
daysToParty curDay   = fromEnum Friday - fromEnum curDay

------------
-- Task 2 --
------------
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

  n * Z     = Z
  n * (S m) = (n * m) + m

  abs = id

  signum Z = 0
  signum _ = 1

  fromInteger x
    | x == 0 = Z
    | x > 0  = S $ fromInteger $ x - 1
    | x < 0  = error "Invalid argument"

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
    | x == 0 = Z
    | x > 0  = S $ toEnum $ x - 1
    | x < 0  = error "Invalid argument"

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
    toIntegerHelp Z acc = acc
    toIntegerHelp (S m) acc = toIntegerHelp m (acc + 1)

  quotRem _ Z = error "Division by zero"
  quotRem n m
    | (n < m)   = (0, n)
    | otherwise =
      let (q, r) = quotRem (n - m) m
      in (q + 1, r)

------------
-- Task 3 --
------------
data Tree a
  = Nil
  | Node (NonEmpty a) (Tree a) (Tree a)
  deriving (Show)

isEmpty :: (Ord a) => Tree a -> Bool
isEmpty Nil = True
isEmpty _   = False

size :: (Ord a) => Tree a -> Int
size Nil           = 0
size (Node xs l r) = length xs + size l + size r

lookup :: (Ord a) => a -> Tree a -> Maybe a
lookup _ Nil = Nothing
lookup e (Node (x :| _) l r) =
  case compare e x of
    LT -> lookup e r
    GT -> lookup e l
    EQ -> Just x

contains :: (Ord a) => a -> Tree a -> Bool
contains e tree =
  case lookup e tree of
    Nothing -> False
    _       -> True

singleton :: (Ord a) => a -> Tree a
singleton e = Node (e :| []) Nil Nil

insert :: (Ord a) => a -> Tree a -> Tree a
insert e Nil = singleton e
insert e (Node list@(x :| xs) l r) =
  case compare e x of
    LT -> (Node list (insert e l) r)
    GT -> (Node list l (insert e r))
    EQ -> (Node (e <| list) l r)

delete :: (Ord a) => a -> Tree a -> Tree a
delete _ Nil = Nil
delete e (Node list@(x :| xs) l r) =
  case compare e x of
    LT -> (Node list (delete e l) r)
    GT -> (Node list l (delete e r))
    EQ -> case xs of
            (xx : xxs) -> (Node (xx :| xxs) l r)
            [] -> unify l r
  where
    unify :: Ord a => Tree a -> Tree a -> Tree a
    unify Nil  r   = r
    unify l    Nil = l
    unify l    r   = case l of
      (Node lvals ll lr) -> (Node lvals ll (unify lr r))

fromList :: Ord a => [a] -> Tree a
fromList  = foldr insert  Nil

{-
bigTree = fromList [1,2,3,4,5,6,1,1,1,100]
newTree = insert  0  (insert (-1) bigTree)
 without1 = 1 `delete`(1 `delete` (1 `delete` (1 `delete` newTree)))
-}
