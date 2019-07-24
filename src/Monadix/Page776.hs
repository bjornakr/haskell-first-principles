module Monadix.Page776 where

data Sum a b =
    First a
  | Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure = Second
  (<*>) (First a) _ = First a
  (<*>) _ (First a) = First a
  (<*>) (Second f) (Second b) = Second (f b)

instance Monad (Sum a) where
  return = pure
  -- (>>=) sa f = (pure f) <*> sa  <- hvorfor funker ikke dette?
  (>>=) (First a) _ = First a
  (>>=) (Second b) f = f b

testSum = do
  s1 <- Second "a "
  s2 <- Second "nice "
  s3 <- Second "test "
  return (s1 ++ s2 ++ s3)

testSum2 = do
  s1 <- Second "a "
  s2 <- Second "nice "
  s3 <- First "FAIL!"
  return (s1 ++ s2 ++ s3)

testSum3 = do
  s1 <- First "FAIL!"
  s2 <- Second "Doesn't matter"
  return (s1 ++ s2)
