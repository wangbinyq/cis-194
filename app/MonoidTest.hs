module TestMonoid where
  import Control.Monad
  import Data.Monoid
  import Test.Hspec
  import Test.QuickCheck

  data Optional a =
    Nada
    | Only a
    deriving (Eq, Show)

  instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend Nada a = a
    mappend b Nada = b
    mappend (Only a) (Only b) = Only (mappend a b)

  dividedBy :: Integral a => a -> a -> (a, a)
  dividedBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)

  monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
  monoidAssoc a b c = a <> (b <> c) == (a <> b) <> c

  monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
  monoidLeftIdentity a = (mempty <> a) == a

  monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
  monoidRightIdentity a = (a <> mempty) == a

  data Bull =
    Fools
    | Twoo
    deriving (Eq, Show)
  
  instance Arbitrary Bull where
    arbitrary =
      frequency [ (1, return Fools)
                , (1, return Twoo) ]
  
  instance Monoid Bull where
    mempty = Fools
    mappend _ _ = Fools

  type BullMappend = Bull -> Bull -> Bull -> Bool

  newtype First' a =
    First' {getFirst' :: Optional a}
    deriving (Eq, Show)
  
  instance Monoid (First' a) where
    mempty = First' Nada
    mappend (First' Nada) a = a
    mappend a _ = a

  instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = do
      x <- arbitrary
      frequency [ (1, return (First' (Only x)))
              , (1, return (First' Nada))]

  firstMappend :: First' a -> First' a -> First' a
  firstMappend = mappend

  type FirstMappend =
    First' String
            -> First' String
            -> First' String
            -> Bool
  type FstId =
    First' String -> Bool


  data Trivial = Trivial deriving (Eq, Show)

  instance Monoid Trivial where
    mempty = Trivial
    mappend Trivial Trivial = Trivial

  instance Arbitrary Trivial where
    arbitrary = 
      frequency [ (1, return Trivial) ]
      
  type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

  newtype Identity a = Identity a deriving Show
  
  instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty
    mappend (Identity a) (Identity b) = Identity (mappend a b)

  testMonoid :: IO ()
  testMonoid = do
    quickCheck (monoidAssoc :: TrivialAssoc)
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)