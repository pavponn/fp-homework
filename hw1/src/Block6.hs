module Block6
  ( Parser(..)
  , ok
  , eof
  , satisfy
  , element
  , stream
  , cbsParser
  , intParser
  , listlistParser
  ) where

import Control.Applicative
import Data.Char (digitToInt, isDigit, isSpace)

data Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  fmap f p =  Parser fun where
    fun dat =
      case (runParser p dat) of
        (Just (a, dat')) -> Just (f a, dat')
        Nothing         -> Nothing

instance Applicative (Parser s) where
  pure a = Parser $ \s -> Just (a, s)
  pf <*> pa =
    Parser $ \dat -> case (runParser pf dat) of
      Just (f, dat') -> runParser (f <$> pa) dat'
      Nothing        -> Nothing

instance Alternative (Parser s) where
  empty     = Parser $ \_  -> Nothing
  pf <|> pa =
    Parser $ \dat ->
      let res = runParser pf dat
      in case res of
        Just _  -> res
        Nothing -> runParser pa dat

instance Monad (Parser s) where
  return = pure
  pa >>= pf =
    Parser $ \dat -> case runParser pa dat of
      Just (a, dat') -> runParser (pf a) dat'
      Nothing        -> Nothing

-- |Parser that never fails and doesn't conusme input.
ok :: Parser s ()
ok = Parser $ \dat -> Just ((), dat)

-- |Parser that checks whether it's the end of the input.
eof :: Parser s ()
eof = Parser $ \dat -> case dat of
  [] -> Just ((), [])
  _  -> Nothing

-- |Parser that checks whether stream's element satisfies predicate.
satisfy :: (s -> Bool) -> Parser s s
satisfy pr = Parser $ \dat -> case dat of
  []     -> Nothing
  (x:xs) -> if pr x then Just (x, xs) else Nothing

-- |Parser that checks wheter stream's element is equal to specified element.
element :: (Eq s) => s -> Parser s s
element e = satisfy (== e)

-- |Parser that checks wheter stream's elements are equal to specified elements.
stream :: (Eq s) => [s] -> Parser s [s]
stream st = case st of
  []     -> return []
  (x:xs) -> pure (:) <*> element x <*> stream xs

-- |Parser of correct bracket sequence.
cbsParser :: Parser Char ()
cbsParser = cbs *> eof where
  cbs :: Parser Char ()
  cbs = (element '(' *> cbs *> element ')' *> cbs) <|> ok

digit :: Parser Char Int
digit = digitToInt <$> satisfy isDigit

digits :: Parser Char [Int]
digits = some digit

nat :: Parser Char Int
nat = foldl (\a b -> a * 10 + b) 0 <$> digits

-- |Parser for integer numbers (with `+` or `-` ahead).
intParser :: Parser Char Int
intParser = negate <$> (element '-' *> nat) <|> (element '+' *> nat) <|> nat

skip :: (s -> Bool) -> Parser s ()
skip p = Parser $ \dat -> case dat of
  []     -> Nothing
  (x:xs) -> if (p x) then Just ((), xs) else Nothing

skipMany :: (s -> Bool) -> Parser s ()
skipMany p = skip p *> skipMany p <|> pure ()

skipElement :: (Eq s) => s -> Parser s ()
skipElement e = skip (== e)

skipSpaces :: Parser Char ()
skipSpaces = skipMany isSpace

skipCharAndSpaces :: Char -> Parser Char ()
skipCharAndSpaces e = skipSpaces *> skipElement e *> skipSpaces

skipComma :: Parser Char ()
skipComma = skipCharAndSpaces ','

listOfSize :: Int -> Parser Char [Int]
listOfSize n
  | n <  0    = Parser $ \_ -> Nothing
  | n == 0    = pure []
  | n == 1    = (:[]) <$> (skipComma *> intParser)
  | otherwise =
    (:) <$> (skipComma *> intParser) <*> listOfSize (n - 1)

listParser :: Parser Char [Int]
listParser = skipSpaces *> intParser >>= \n -> listOfSize n

-- |Parser of 2-dimensional integer lists.
-- ex: "2, 1,+10  , 3,5,-7, 2" -> [[1, 10], [5, -7, 2]]
listlistParser :: Parser Char [[Int]]
listlistParser =
  listParser >>= \list ->
  (skipComma *> listlistParser <|> endParser) >>= \lists ->
  skipSpaces *> return (list : lists)
  <|>
  skipSpaces *> eof *> pure []
  where
    endParser :: Parser Char [[Int]]
    endParser = skipSpaces *> eof *> pure []

-- skipComma *> listlistParser <|> skipSpaces *> eof *> pure []
