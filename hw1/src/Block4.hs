module Block4
  ( stringSum
  , Tree (..)
  ) where

import Block3 (NonEmpty (..))
import Text.Read (readMaybe)

-- |Calculates ssum of Ints that occurr in the string.
stringSum :: String -> Maybe Int
stringSum s = traverse readMaybe (words s) >>= Just . sum

-- |Represents simple binary tree.
data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a
  deriving (Show)

instance Functor Tree where
  fmap f (Leaf a)     = Leaf $ f a
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

instance Applicative Tree where
  pure  = Leaf
  (Leaf f)     <*> t = fmap f t
  (Branch f g) <*> t = Branch (f <*> t) (g <*> t)

instance Foldable Tree where
  foldMap f (Leaf a)     = f a
  foldMap f (Branch l r) = (foldMap f l) <> (foldMap f r)

instance Traversable Tree where
  traverse f (Leaf a)     = pure Leaf <*> f a
  traverse f (Branch l r) = pure Branch <*> traverse f l <*> traverse f r

-- treeToList :: Tree a -> [a]
-- treeToList t = foldr (:) [] t

instance Functor NonEmpty where
  fmap f (x :| xs) = f x :| (fmap f xs)

instance Applicative NonEmpty where
  pure = (:| [])
  (f :| fs) <*> (x :| xs) = (f x) :| (fmap f xs ++ (fs <*> (x:xs)))

instance Monad NonEmpty where
  (x :| xs) >>= f = (x' :| (xs' ++ xs'')) where
    (x' :| xs') = f x
    xs'' = xs >>= toList . f
    toList :: NonEmpty a -> [a]
    toList (a :| as) = (a : as)

instance Foldable NonEmpty where
  foldr f ini (x :| xs) = f x (foldr f ini xs)

instance Traversable NonEmpty where
  traverse f (x :| xs) = (:|) <$> f x <*> traverse f xs
