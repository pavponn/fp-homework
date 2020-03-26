module BlockBonus
  ( Cont (..)
  ) where

import Control.Monad (ap)

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Functor (Cont r) where
  fmap f x = pure f <*> x

instance Applicative (Cont r) where
  pure  = return
  (<*>) = ap

instance Monad (Cont r) where
  return x = Cont $ \c -> c x

  Cont v >>= k = Cont $ \c -> v (\a -> runCont (k a) c)

-- Spent some time studying this monad using lecture slides and Stepik course.
-- Basic functionality is understandable, but I have some problems with task.
-- Probably will complete it after the deadline :/
