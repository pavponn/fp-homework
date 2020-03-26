{-# LANGUAGE InstanceSigs #-}

module Block2
  ( splitOn
  , joinWith
  )
  where

import Block1 (Tree (..))
import Data.List.NonEmpty (NonEmpty (..))

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ ini Nil           = ini
  foldr f ini (Node vs l r) = foldr f (foldr f (foldr f ini r) vs) l

  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Nil           = mempty
  foldMap f (Node vs l r) = foldMap f l <> foldMap f vs <> foldMap f r

-- |Splits list of elements into NonEmpty (of lists) by given element.
splitOn :: (Eq a) => a -> [a] -> NonEmpty [a]
splitOn element = foldr helper ([] :| []) where
  helper = \c (x :| xs) -> if c == element
    then []:| (x : xs)
    else (c:x) :| xs

-- |Joins NonEmpty (of lists) using given element.
joinWith :: a -> NonEmpty [a] -> [a]
joinWith element (x :| xs) = x ++ foldr helper [] xs where
  helper = \c cs -> (element : c) ++ cs
