{-# LANGUAGE FlexibleInstances #-}

module Fx.Page680 where


-- Rearrange the arguments to the type constructor of the datatype so the Functor instance works.

-- 1
data Sum b a =
    First a
  | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b



-- 2
data Company c a b =  -- the order of 'c' and 'a' does not matter
    DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c



data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'


testMore = (fmap (+1) (L 1 2 3) == L 2 2 4) &&
           (fmap (+1) (R 1 2 3) == R 1 3 3)


data Quant a b =
    Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap f (Bloor b) = Bloor (f b)
  fmap _ Finance = Finance
--  fmap _ d@(Desk a) = d -- Doesn't work. Why?
  fmap _ (Desk a) = Desk a



-- 2.
data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K a) = K a




-- 3.
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
--newtype Kf a b = Kf a

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip (K (f a))

testFlip = fmap (+1) (Flip (K 1)) == Flip (K 2)


