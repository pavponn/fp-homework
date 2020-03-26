module Block3
  ( maybeConcat
  , eitherConcat
  , NonEmpty(..)
  , ThisOrThat(..)
  , Name(..)
  , Endo(..)
  )where

-- |Concats list of lists inside Maybe in one list.
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = foldr ((<>) . concat) []

-- |Concats list of monoids packed into Left/Right in pair.
eitherConcat :: (Monoid m1, Monoid m2) => [Either m1 m2] -> (m1, m2)
eitherConcat = foldr helper (mempty, mempty) where
  helper :: (Monoid m1, Monoid m2) => Either m1 m2 -> (m1, m2) -> (m1, m2)
  helper (Left l)  (lm, rm) = (l <> lm, rm)
  helper (Right r) (lm, rm) = (lm, r <> rm)

-- |NonEmpty - list, but always has at least one element.
data NonEmpty a = a :| [a]
  deriving (Eq, Show)

instance Semigroup (NonEmpty a) where
  (x :| xs) <> (y :| ys) = x :| (xs ++ y : ys)

-- |ThisOrThat - contains element of one of two types or both&
data ThisOrThat a b
  = This a
  | That b
  | Both a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (ThisOrThat a b) where
  This x1    <> This x2    = This (x1 <> x2)
  This x     <> That y     = Both x          y
  This x1    <> Both x2 y  = Both (x1 <> x2) y
  That y     <> This x     = Both x          y
  That y1    <> That y2    = That (y1 <> y2)
  That y1    <> Both x y2  = Both x          (y1 <> y2)
  Both x1 y  <> This x2    = Both (x1 <> x2) y
  Both x  y1 <> That y2    = Both x          (y1 <> y2)
  Both x1 y1 <> Both x2 y2 = Both (x1 <> x2) (y1 <> y2)

newtype Name = Name String deriving (Eq, Show)

instance Semigroup Name where
  Name "" <> Name s  = Name s
  Name s  <> Name "" = Name s
  Name s1 <> Name s2 = Name $ s1 ++ "." ++ s2

instance Monoid Name where
  mempty = Name ""

newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
  Endo f <> Endo g = Endo $ f . g

instance Monoid (Endo a) where
  mempty = Endo id
