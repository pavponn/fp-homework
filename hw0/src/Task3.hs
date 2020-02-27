module Task3
  ( s
  , composition
  , identity
  , contraction
  , permutation
  ) where

-- |This function represents S combinator, `const` is K combinator.
s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

-- |This function represents B combinator, using SK basis B = (S(KS))K.
composition :: (b -> c) -> (a -> b) -> a -> c
composition = (s (const s)) const

-- |This function represents I combinator, using SK basis I = SKK.
identity :: a -> a
identity = s const const

-- |This function represents W combinator, using SK basis W = SS(SK).
contraction :: (a -> a -> b) -> a -> b
contraction = s s (s const)

-- |This function represents W combinator, using SK basis
-- C = S(S(K(S(KS)K))S)(KK).
permutation :: (a -> b -> c) -> b -> a -> c
permutation = s (s (const (s (const s) const)) s) (const const)
