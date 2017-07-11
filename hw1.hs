toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <=0 = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x : y)
  | length y `mod` 2 == 0 = x : doubleEveryOther(y)
  | otherwise = (2 * x) : doubleEveryOther(y)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x]
  | x < 10 = x
  | otherwise = sumDigits (toDigits x)
sumDigits (x:y) = sumDigits([x]) + sumDigits(y)

validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther (toDigits n))) `mod` 10 == 0


type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi n a b c = (hanoi (n-1)  a  c  b) ++ [(a, b)] ++ (hanoi (n-1)  c  b  a)

-- nHanoi :: Integer -> [Peg] -> [Move]
-- nHanoi 0 _ = []
-- nHanoi 1 (p1 : p2 : rest) = [(p1, p2)]
-- nHanoi n (p1 : p2 : p3 : rest) =
  