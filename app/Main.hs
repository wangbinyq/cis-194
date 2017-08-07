module Main where

printSecond :: String -> IO ()
printSecond greeting = do
  putStrLn greeting

main :: IO ()
main = do
  putStrLn greeting
  printSecond greeting
    where greeting = "Yarrrrr"


length' :: [a] -> Int
length' [] = 0
length' (a : b) = 1 + (length' b)

data Trivial = Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True

data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun
  deriving (Eq, Ord, Enum, Show)

data Date =
  Date DayOfWeek Int
  deriving (Eq, Ord, Show)

data Identity a =
  Identity a
  deriving Show

instance Ord a => Eq (Identity a) where
  (Identity x) == (Identity y) = x == y

data NoEqInst = NoEqInst


class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer

newtype Age =
  Age Integer
  deriving (Show, Eq)

instance Numberish Age where
  fromNumber a = Age a
  toNumber (Age a) = a

newtype Year =
  Year Integer
  deriving (Show, Eq)

instance Numberish Year where
  fromNumber a = Year a
  toNumber (Year a) = a

sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed
  where integerOfA = toNumber a
        integerOfAPrime = toNumber a'
        summed = integerOfA + integerOfAPrime

isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False

newtype Username = Username String deriving Show
newtype AccountNumber = AccountNumber Integer deriving Show
data User = UnregisteredUser
            | RegisteredUser Username AccountNumber
            deriving Show

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name)
                          (AccountNumber acctNum))
          = putStrLn $ name ++ " " ++ show acctNum


dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where y = x / 100

str = "all i wanna do is have some fun"
myWords :: String -> [String]
myWords "" = []
myWords (' ' : str) = myWords str
myWords str = takeWhile (/= ' ') str : (myWords $ dropWhile (/= ' ') str)

split :: Eq a => a -> [a] -> [[a]]
split a [] = []
split a list@(c : t)
  | a == c = split a t
  | otherwise = takeWhile (/= a) list : (split a $ dropWhile (/= a) list)

mySqr = [x^2 | x <- [1..5]]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f _ [] = []
zipWith' f [] _ = []
zipWith' f (a:as) (b:bs) = (f a b) : (zipWith' f as bs)

zip' = zipWith' (\a b -> (a, b))

-- foldr' :: (a -> b -> b) -> b -> [a] -> b
-- foldr' _ b [] = b
-- foldr' f b (a: as) = a `f` foldr' f b as

-- foldl' :: (b -> a -> b) -> b -> [a] -> b
-- foldl' _ b [] = b
-- foldl' f b (a : as) = foldl' f (f b a) as

fibs = 1 : scanl (+) 1 fibs

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42