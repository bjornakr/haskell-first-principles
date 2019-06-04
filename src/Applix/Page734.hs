module Applix.Page734 where

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)


append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- Remember what you wrote for the list Functor:
instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a as)  = Cons (f a) (fmap f as)

-- Writing the list Applicative is similar.
instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f fs) as = (fmap f as) `append` (fs <*> as)

