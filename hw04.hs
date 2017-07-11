fun1' :: [Integer] -> Integer
fun1' = product . map (-2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter event . takeWhile (/=1) . iterate (\n if event n then n `div` 2 else 3*n+1) 