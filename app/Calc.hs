module Calc where
  import ExprT
  import Parser

  newtype MinMax = MinMax Integer deriving (Eq, Show)
  newtype Mod7 = Mod7 Integer deriving (Eq, Show)

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

  instance Expr Integer where
      lit a = a
      add a b = a + b
      mul c d = c * d

  instance Expr Bool where
    lit a = a > 0
    add = (||)
    mul = (&&)

  instance Expr MinMax where
    lit a = (MinMax a)
    add ma@(MinMax a) mb@(MinMax b) = if a > b then ma else mb
    mul ma@(MinMax a) mb@(MinMax b) = if a < b then ma else mb

  instance Expr Mod7 where
    lit a = (Mod7 (mod a 7))
    add ma@(Mod7 a) mb@(Mod7 b) = lit (a + b)
    mul ma@(Mod7 a) mb@(Mod7 b) = lit (a * b)


  reify :: ExprT -> ExprT
  reify = id


  testExp :: Expr a => Maybe a
  testExp = parseExp lit add mul "(3 * -4) + 5"
  testInteger = testExp :: Maybe Integer
  testBool = testExp :: Maybe Bool
  testMM = testExp :: Maybe MinMax
  testSat = testExp :: Maybe Mod7