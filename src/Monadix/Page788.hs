module Monadix.Page788 where

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return _ = NopeDotJpg
  (>>=) _ _ = NopeDotJpg

data PxEither b a =
    PxLeft a
  | PxRight b
  deriving (Eq, Show)


instance Functor (PxEither b) where
  fmap f (PxLeft a) = PxLeft (f a)

instance Applicative (PxEither b) where
  pure = PxLeft
  (<*>) (PxRight b) _ = PxRight b
  (<*>) _ (PxRight b) = PxRight b
  (<*>) (PxLeft f) (PxLeft a) = PxLeft (f a)

instance Monad (PxEither b) where
  return = pure
  (>>=) (PxRight b) _ = PxRight b
  (>>=) (PxLeft a) f  = f a
