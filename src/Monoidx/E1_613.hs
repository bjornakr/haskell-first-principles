module Monoidx.E1_613 where

import Test.QuickCheck

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- Trivial
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = pure Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

testTrivial :: IO ()
testTrivial = do
  let 
    sa = semigroupAssoc
    mli = monoidLeftIdentity
    mlr = monoidRightIdentity
  quickCheck (sa :: TrivAssoc)
  quickCheck (mli :: Trivial -> Bool)
  quickCheck (mlr :: Trivial -> Bool)


-- Identity
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a1) <> (Identity a2) = Identity (a1 <> a2)

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    pure (Identity a)

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

testIdentity :: IO ()
testIdentity = do
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)



-- Two
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    pure (Two a b)

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

testTwo :: IO ()
testTwo = do
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)



-- BoolConj
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj b1) <> (BoolConj b2) = BoolConj (b1 && b2)

instance Monoid BoolConj where
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    pure (BoolConj a)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

testBoolConj :: IO ()
testBoolConj = do
  putStrLn "quickCheck (semigroupAssoc :: BoolConjAssoc)"
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  putStrLn "quickCheck (monoidLeftIdentity :: BoolConj -> Bool)"
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  putStrLn "quickCheck (monoidRightIdentity :: BoolConj -> Bool)"
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)


-- Combine
newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f1) <> (Combine f2) = Combine (\x -> (f1 x) <> (f2 x)) 

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine (\_ -> mempty)


-- Comp 
newtype Comp a = Comp { unComp :: (a -> a)}

instance Semigroup (Comp a) where
  (Comp f1) <> (Comp f2) = Comp $ \x -> f1 (f2 x) -- TODO: Verifiser

instance Monoid a => Monoid (Comp a) where
  mempty = Comp mempty

data Validation a b = Failuri a | Successi b deriving (Eq, Show)
instance Semigroup a => Semigroup (Validation a b) where
  Failuri a1 <> Failuri a2 = Failuri (a1 <> a2)
  Failuri _ <> s@(Successi _) = s
  s@(Successi _) <> Failuri _ = s 
  s@(Successi b1) <> Successi b2 = s
  
compi = do
  let 
    failure :: String -> Validation String Int
    failure = Failuri
    success :: Int -> Validation String Int
    success = Successi
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2

main :: IO ()
main = do
  testTrivial
  testIdentity
  testTwo
  testBoolConj


  -- let a = arbitrary::(Gen BoolConj)
  -- sample a