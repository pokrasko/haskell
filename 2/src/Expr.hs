module Expr where

data Operation
    = Plus
    | Minus
    | Multiply
    | Divide
    | Power

applyOperation :: Operation -> (Int -> Int -> Int)
applyOperation Plus = (+)
applyOperation Minus = (-)
applyOperation Multiply = (*)
applyOperation Divide = div
applyOperation Power = (^)

data Expr = Const Int
          | App Operation Expr Expr

type ArithmeticError = String
type Result = Either ArithmeticError Int

eval :: Expr -> Result
eval (Const x) = Right x
eval (App f x y) = tryEval f (eval x) (eval y)
  where
    tryEval :: Operation -> Result -> Result -> Result
    tryEval Divide _ (Right 0) = Left "Division by 0!"
    tryEval f x y = applyOperation f <$> x <*> y
