-- functions
hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3*n + 1

foo :: Integer -> Integer
foo 0 = 16
foo 1 
  | "Haskell" > "C++" = 3
  | otherwise         = 4
foo n
  | n < 0            = 0
  | n `mod` 17 == 2  = -43
  | otherwise        = n + 3

isEvent :: Integer -> Bool
isEvent n
  | mod n 2 == 0 = True
  | otherwise = False

-- Pairs
p :: (Int, Char)
p = (3, 'x')

-- multiple arguments
f::Int->Int -> Int -> Int
f x y z = x + y + z

-- lists
nums, range, range2 :: [Integer]
nums   = [1,2,3,19]
range  = [1..100]
range2 = [2,4..100]

-- list comprehensions+
nums2 :: [Integer]
nums2 = [x*2 | x <- nums]

-- constructing list
-- cons operator `:`
ex18 = 1 : []
ex19 = 3 : (1 : [])
ex20 = 2 : 3 : 4 : []
ex21 = [2,3,4] == 2 : 3 : 4 : []


hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)


-- functions on lists
-- Compute the length of a list of Integers.
intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength (x:xs) = 1 + intListLength xs

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []         = []     -- Do nothing to the empty list
sumEveryTwo (x:[])     = [x]    -- Do nothing to lists with a single element
sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs -- sumEveryTwo (x:y:zs) = will ok

fib :: Integer -> Integer
fib n
  | n < 2 = n
  | otherwise = fib (n - 1) + fib (n -2)  