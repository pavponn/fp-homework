{-# LANGUAGE TypeOperators #-}

module Task1
  ( associator
  , distributivity
  , eitherAssoc
  ) where

-- |This function represents distributivity.
distributivity :: Either a (b, c) -> (Either a b, Either a c)
distributivity (Left x)  = (Left x, Left x)
distributivity (Right y) = (Right $ fst y, Right $ snd y)

-- |This function represents association.
associator :: (a, (b, c)) -> ((a, b), c)
associator (x, (y, z)) = ((x, y), z)

type (<->) a b = (a -> b, b -> a)

-- |This function represents Either association.
eitherAssoc :: Either a (Either b c) <-> Either (Either a b) c
eitherAssoc = (assocFromLeft, assocFromRight)
  where
    assocFromLeft :: Either a (Either b c) -> Either (Either a b) c
    assocFromLeft (Left x)          = Left (Left x)
    assocFromLeft (Right (Left x))  = Left (Right x)
    assocFromLeft (Right (Right x)) = Right x

    assocFromRight :: Either (Either a b) c -> Either a (Either b c)
    assocFromRight (Left (Left x))  = Left x
    assocFromRight (Left (Right x)) = Right (Left x)
    assocFromRight (Right x)        = Right (Right x)
