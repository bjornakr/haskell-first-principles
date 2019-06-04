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


-- 2 --
-- data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure a = Two mempty a
  (<*>) (Two a1 f) (Two a2 b) = Two (a1 <> a2) (f b)

