-- enumeration types
data Thing = Shoe 
           | Ship 
           | SealingWax 
           | Cabbage 
           | King
  deriving Show -- magical automatically generate default code for converting Things to Strings

-- algebraic data types
-- FailableDouble have two data constructor
data FailableDouble = Failure
                    | OK Double
  deriving Show

-- enumeration types is specical case of algebraic data types

-- data constructors can have more than one agument
-- type constructor and data constructor botn named Person, but they are different
data Person = Person String Int Thing
  deriving Show

brent :: Person
brent = Person "Brent" 31 SealingWax

stan :: Person
stan  = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a

-- adt in general
-- data AlgDataType = Constr1 Type11 Type12
--                  | Constr2 Type21
--                  | Constr3 Type31 Type32 Type33
--                  | Constr4

-- pattern matching
-- Fundamentally, pattern-matching is about taking apart a value
-- by finding out which constructor it was built with
-- foo (Constr1 a b)   = ...
-- foo (Constr2 a)     = ...
-- foo (Constr3 a b c) = ...
-- foo Constr4         = ...

-- An underscore _ can be used as a “wildcard pattern” which matches anything.
-- A pattern of the form x@pat can be used to match a value against the pattern pat,
   -- but also give the name x to the entire value being matched.
-- Patterns can be nested

-- we can pattern match against literal values is
-- data Int  = 0 | 1 | -1 | 2 | -2 | ...
-- data Char = 'a' | 'b' | 'c' | ...

-- Case expressions
failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
                     Failure -> 0
                     OK d    -> d