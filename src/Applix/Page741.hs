{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Applix.Page741 where

import Data.Monoid ((<>))
import Control.Applicative (liftA3)
import Monoidx.E1_613

-- 1 --
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f1 f2) (Pair a1 a2) = (Pair (f1 a1) (f2 a2))

pairTest = ((Pair (+1) (*2)) <*> Pair 10 20) == Pair 11 40

-- 2 --
-- data Two a b = Two a b deriving (Eq, Show)

-- instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
--   (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

-- instance (Monoid a, Monoid b) => Monoid (Two a b) where
--   mempty = Two mempty mempty

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure a = Two mempty a
  (<*>) (Two a1 f) (Two a2 b) = Two (a1 <> a2) (f b)

twoTest = ((Two "x" (*2)) <*> (Two "y" 10)) == Two "xy" 20


stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (\a b c -> (a, b, c))

