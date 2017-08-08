module FunctorTest where
  import Data.Functor
  import Test.Hspec
  import Test.QuickCheck

  data FixMePls a =
    FixMe
    | Pls a
    deriving (Eq, Show)

  instance Functor FixMePls where
    fmap _ FixMe = FixMe
    fmap f (Pls a) = Pls (f a)

  data WhoCares a =
    ItDoesnt
    | Matter a
    | WhatThisIsCalled
    deriving (Eq, Show)

  instance Functor WhoCares where
    fmap _ ItDoesnt = ItDoesnt
    fmap _ WhatThisIsCalled = WhatThisIsCalled
    fmap f (Matter a) = Matter (f a)

  functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
  functorIdentity f = fmap id f == f

  functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
  functorCompose f g x = fmap g (fmap f x) == fmap (g . f) x

  data Possibly a =
    LolNope
    | Yeppers a
    deriving (Eq, Show)

  instance Functor Possibly where
    fmap _ LolNope = LolNope
    fmap f (Yeppers a) = Yeppers (f a)

  data Wrap f a =
    Wrap (f a)
    deriving (Eq, Show)

  instance Functor f => Functor (Wrap f) where
    fmap f (Wrap fa) = Wrap (fmap f fa)

  getInt :: IO Int
  getInt = fmap read getLine


  data Sum a b =
    First a
    | Second b

  instance Functor (Sum e) where
    fmap f (First a) = First a
    fmap f (Second b) = Second (f b)

  data Company a b c =
    DeepBlue a c
    | Something b

  instance Functor (Company e e') where
    fmap _ (Something b) = Something b
    fmap f (DeepBlue a c) = DeepBlue a (f c)

  data More b a =
    L a b a
    | R b a b
    deriving (Eq, Show)
    
  instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'

  data Quant a b =
    Finance
    | Desk a
    | Bloor b
    deriving (Eq, Show)

  instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap _ (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)