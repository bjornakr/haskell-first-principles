module Monoidx.E1_613 where
  import Test.QuickCheck

  data Trivial = Trivial deriving (Eq, Show)
  
  instance Semigroup Trivial where
    _ <> _ = Trivial

  newtype Identity a = Identity a deriving (Eq, Show)

  instance Semigroup a => Semigroup (Identity a) where
    (Identity a1) <> (Identity a2) = Identity (a1 <> a2)

  data Two a b = Two a b deriving (Eq, Show)
  instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

  instance Arbitrary Trivial where
    arbitrary = pure Trivial

  instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
      a <- arbitrary
      pure (Identity a)

  instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      pure (Two a b)

  semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
  semigroupAssoc a b c =
    (a <> (b <> c)) == ((a <> b) <> c)

  type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

  type IdAssoc = Identity String -> Identity String -> Identity String -> Bool

  type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool


  newtype BoolConj = BoolConj Bool deriving (Eq, Show)
  instance Semigroup BoolConj where
    (BoolConj b1) <> (BoolConj b2) = BoolConj (b1 && b2)

  instance Arbitrary BoolConj where
    arbitrary = do
      a <- arbitrary
      pure (BoolConj a)

  type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool


  newtype Combine a b = Combine { unCombine :: (a -> b) }
  instance Semigroup (Combine a b) where
    (Combine f1) <> (Combine f2) = Combine (\x -> f2 (f1 x)) 

  main :: IO ()
  main = do
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (semigroupAssoc :: IdAssoc)
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (semigroupAssoc :: BoolConjAssoc)


    -- let a = arbitrary::(Gen BoolConj)
    -- sample a