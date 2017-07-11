skip :: [a] -> Int -> [a]
skip [] _ = []
skip _ 0 = []
skip xs n = [xs!!(i - 1) | i <- [1..(length xs)], (i `mod` n) == 0]

skips :: [a] -> [[a]]
skips xs = [skip xs i | i <- [1..length xs]]


localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
  | x < y && y > z = y : localMaxima(y : z : zs)
  | otherwise = localMaxima(y : z : zs)
localMaxima _ = []


histogram :: [Integer] -> String
histogram xs = unlines (map (line c) [m+1,m..1]) ++ "==========\n0123456789\n"
  where c = count xs
        m = maximum c

-- returns one * line from the above function
line :: [Int] -> Int -> String
line xs n = [if i >= n then '*' else ' ' | i <- xs]

-- counts occurence of numbers in [0..9] in the input list.
count :: [Integer] -> [Int]
count xs = map (\n -> length $ filter (== n) xs) [0..9]
