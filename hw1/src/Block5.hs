module Block5
  ( Expr(..)
  , ArithmeticError(..)
  , eval
  , moving
  ) where

import Control.Monad.State (State, evalState, get, put)
import Data.Sequence hiding (reverse)
import Prelude hiding (length)

-- |Type that represents arithmetic expression.
data Expr
  = Const Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  deriving (Show)

-- |Type that represents arithmetic error.
data ArithmeticError
  = DivisionByZeroError
  | NegativePowerError
  deriving (Eq, Show)

type ErrorOrInt = Either ArithmeticError Int

-- |Evaluates given expression, return it's value packed in Right.
-- If error occurred returns Left ArithmeticError.
eval :: Expr -> ErrorOrInt
eval expr =
  case expr of
    (Const x)    -> Right x
    (Add  e1 e2) -> evalOp (simpleOp (+)) e1 e2
    (Sub  e1 e2) -> evalOp (simpleOp (-)) e1 e2
    (Mul  e1 e2) -> evalOp (simpleOp (*)) e1 e2
    (Div  e1 e2) -> evalOp safeDiv        e1 e2
    (Pow  e1 e2) -> evalOp safePow        e1 e2
  where
    evalOp :: (Int -> Int -> ErrorOrInt) -> Expr -> Expr -> ErrorOrInt
    evalOp op e1 e2 =
      eval e1 >>= \x ->
      eval e2 >>= \y ->
      x `op` y

    simpleOp :: (Int -> Int -> Int) -> Int -> Int -> ErrorOrInt
    simpleOp op = \x y -> Right $ x `op` y

    safeDiv :: Int -> Int -> ErrorOrInt
    safeDiv x y =
      if y == 0 then Left DivisionByZeroError
      else Right $ x `div` y

    safePow :: Int -> Int -> ErrorOrInt
    safePow x y =
      if y < 0 then Left NegativePowerError
      else Right $ x ^ y

data AlgorithmState t = AlgorithmState
  { remainList  :: [t]
  , algoSize    :: Int
  , algoQueue   :: Seq t
  , algoCurList :: [t]
  , algoCurSum  :: t
  , algoIndex   :: Int
  }

algoStateInit :: (Fractional t) => Int -> [t] -> AlgorithmState t
algoStateInit size list =
  AlgorithmState
  { remainList  = list
  , algoSize    = size
  , algoQueue   = empty
  , algoCurList = []
  , algoCurSum  = 0
  , algoIndex   = 1
  }

-- |Returns result for given int and list
-- calculated by simple moving average algorithm.
moving :: (Fractional t) => Int -> [t] -> [t]
moving size list
  | size <= 0 = error "Positive integer expected as an argument"
  | otherwise =
      let algoState = algoStateInit size list
      in reverse $ evalState simpleMovingAlgorithm algoState

simpleMovingAlgorithm :: (Fractional t) => State (AlgorithmState t) [t]
simpleMovingAlgorithm = do
  (AlgorithmState list sz queue res curSum curIndex) <- get
  case list of
    []     -> return res
    (x:xs) -> do
      let (val, q)  = getAndDelete queue (sz - 1)
      let newSum    = curSum + x - val
      let newResult = newSum / (fromIntegral $ min sz curIndex) : res
      let newQueue  = x <| q
      put (AlgorithmState xs sz newQueue newResult newSum (curIndex + 1))
      simpleMovingAlgorithm

getAndDelete :: (Fractional t) => Seq t -> Int -> (t, Seq t)
getAndDelete s i =
  if i >= length s
    then (0, s)
  else (index s i, deleteAt i s)
