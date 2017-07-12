module Calc where
  import ExprT
  import Parser

  eval :: ExprT -> Integer
  eval (Add left right) = eval left + eval right
  eval (Mul left right) = eval left * eval right
  eval (Lit n) = n

  evalStr :: String -> Maybe Integer
  evalStr expr = case parseExp Lit Add Mul expr of
    Nothing -> Nothing
    Just exprt -> Just (eval exprt)

  class Expr a where
    lit :: Integer -> a
    add, mul :: a -> a -> a

  instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

  reify :: ExprT -> ExprT
  reify = id

  