module TestMonoid where
  import Data.Monoid
  import Test.Hspec
  import Test.QuickCheck

  data Optional a =
    Nada
    | Only a
    deriving (Eq, Show)

  instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend Nada a@(_) = a 
    mappend b@(_) Nada = b 
    mappend (Only a) (Only b) = Only (mappend a b)

  dividedBy :: Integral a => a -> a -> (a, a)
  dividedBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)


  asc :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
  asc (<>) a b c =
    a <> (b <> c) == (a <> b) <> c
  
  monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
  monoidAssoc a b c = a <> (b <> c) == (a <> b) <> c
  
