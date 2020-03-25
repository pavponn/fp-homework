module Block5Spec where

import Block5 (Expr(..), ArithmeticError(..), eval, moving)
import Test.Hspec (SpecWith, describe, it, shouldBe)

evalSpec :: SpecWith ()
evalSpec = describe "Block5.eval" $ do
  it "should add simple expressions" $ do
    eval (Add (Const 5) (Const 7)) `shouldBe` Right 12

  it "should mutliply simple expressions" $ do
    eval (Mul (Const 2) (Const 4)) `shouldBe` Right 8

  it "should subtract simple expressions" $ do
    eval (Sub (Const 1) (Const 8)) `shouldBe` Right (-7)

  it "should divide simple expression" $ do
    eval (Div (Const 16) (Const 2)) `shouldBe` Right 8

  it "should power simple expression" $ do
    eval (Pow (Const 2) (Const 10)) `shouldBe` Right 1024

  it "should calculate big expressin" $ do
    eval
      (Add
        (Mul (Const 5) (Const 7))
        (Div (Const 24) (Pow (Const 2) (Const 3)))
      )
    `shouldBe` Right 38

  it "should fail with DivisionByZeroError" $ do
    eval
      (Add
        (Sub (Const 1) (Const 7))
        (Div (Const 24) (Const 0))
      )
    `shouldBe` Left DivisionByZeroError

  it "should fail with NegativePowerError" $ do
    eval
      (Add
        (Add (Const 1) (Const 7))
        (Pow (Const 24) (Sub (Const 1) (Const 7)))
      )
    `shouldBe` Left NegativePowerError

movingSpec :: SpecWith ()
movingSpec = describe "Block5.moving" $ do
  it "moving 4 [1, 5, 3, 8, 7, 9, 6]" $ do
    moving 4 [1 :: Double, 5, 3, 8, 7, 9, 6]
      `shouldBe` [1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5]

  it "moving 2 [1, 5, 3, 8, 7, 9, 6]" $ do
    moving 2 [1 :: Double, 5, 3, 8, 7, 9, 6]
      `shouldBe` [1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]
