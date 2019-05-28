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



-- 4.
data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)



-- ★5★
-- LiftItOut :: f a -> LiftItOut f a
data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap g (LiftItOut fa) = LiftItOut (fmap g fa)

testLiftItOut = fmap (+1) (LiftItOut (Just 1)) == LiftItOut (Just 2)


-- ★6★
-- DaWrappa :: f a -> g a -> Parappa f g a
data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap h (DaWrappa fa ga) = DaWrappa (fmap h fa) (fmap h ga)

testDaWrappa = fmap (+1) (DaWrappa (Just 1) [2]) == DaWrappa (Just 2) [3]


-- ★7★ --
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap h (IgnoringSomething fa gb) = IgnoringSomething fa (fmap h gb)


-- ★8★ --
data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious (go) (ga) (fmap f gt)

-- ★9★ --
data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a (la)) = Cons (f a) (fmap f la)

testList =
  let
    list = Cons 1 (Cons 2 (Cons 3 Nil))
  in
    fmap (*10) list == Cons 10 (Cons 20 (Cons 30 Nil))

