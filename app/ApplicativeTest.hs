module ApplicativeTest where
  import Control.Applicative
  import Data.Monoid
  import Test.Hspec
  import Test.QuickCheck

  data Identity a = Identity a deriving (Eq, Ord, Show)

  instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

  instance Applicative Identity where
    pure a = Identity a
    (Identity f) <*> (Identity a) = Identity (f a)

  data Constant a b = 
    Constant {getConstant :: a}
    deriving (Eq, Ord, Show)

  instance Functor (Constant a) where
    fmap _ (Constant x) = Constant x
  
  instance Monoid a => Applicative (Constant a) where
    pure x = Constant mempty
    (Constant x) <*> (Constant y) = Constant (mappend x y)

  validateLength :: Int -> String -> Maybe String
  validateLength maxLen s =
    if (length s) > maxLen
    then Nothing
    else Just s
  
  newtype Name = Name String deriving (Eq, Show)
  newtype Address = Address String deriving (Eq, Show)

  mkName :: String -> Maybe Name
  mkName s = fmap Name $ validateLength 25 s

  mkAddress :: String -> Maybe Address
  mkAddress s = fmap Address $ validateLength 100 s

  data Person =
    Person Name Address
    deriving (Eq, Show)

  mkPerson :: String -> String -> Maybe Person
  mkPerson n a =
    Person <$> mkName n <*> mkAddress a


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

  cowFromString :: String -> Int -> Int -> Maybe Cow
  cowFromString name' age' weight' =
    Cow <$> name <*> age <*> weight
    where name = noEmpty name'
          age = noNegative age'
          weight = noNegative weight'

  data List a =
    Nil
    | Cons a (List a)
    deriving (Eq, Show)

  instance Monoid (List a) where
    mempty = Nil
    mappend a Nil = a
    mappend Nil a = a
    mappend (Cons x xs) ys = Cons x $ xs `mappend` ys

  instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a b) = Cons (f a) (fmap f b)  

  instance Applicative List where
    pure a = Cons a Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    Cons f b <*> ca = fmap f ca <> (b <*> ca)
    
  