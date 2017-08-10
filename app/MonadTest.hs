module TestMonad where
  import Control.Monad
  import Test.Hspec
  import Test.QuickCheck

  bind :: Monad m => (a -> m b) -> m a -> m b
  bind f a = join $ fmap f a

  twiceWhenEven :: [Integer] -> [Integer]
  twiceWhenEven xs = do
    x <- xs
    if even x
    then [x*x, x*x]
    else []

  data Cow = Cow {
      name :: String
      , age :: Int
      , weight :: Int
    } deriving (Eq, Show)

  noEmpty :: String -> Maybe String
  noEmpty "" = Nothing
  noEmpty str = Just str

  noNegative :: Int -> Maybe Int
  noNegative n | n >= 0 = Just n
               | otherwise = Nothing

  -- if Cow's name is Bess, must be under 500
  weightCheck :: Cow -> Maybe Cow
  weightCheck c =
    let w = weight c
        n = name c
    in if n == "Bess" && w > 499
         then Nothing
         else Just c

  mkSphericalCow :: String -> Int -> Int -> Maybe Cow
  mkSphericalCow name' age' weight' = do
    name <- noEmpty name'
    age <- noNegative age'
    weight <- noNegative weight'
    weightCheck (Cow name age weight)

  f :: Integer -> Maybe Integer
  f 0 = Nothing
  f n = Just n

  g :: Integer -> Maybe Integer
  g i =
    if even i
    then Just (i + 1)
    else Nothing

  h :: Integer -> Maybe String
  h i = Just ("10191" ++ show i)

  doSomething' n = do
    a <- f n
    b <- g a
    c <- h b
    pure (a, b, c)
