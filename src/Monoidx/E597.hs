module Monoidx.E597 where

import Data.Monoid
import Data.Semigroup


data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
    (<>) Nada Nada = Nada
    (<>) a1@(Only _) Nada = a1
    (<>) Nada a2@(Only _) = a2
    (<>) (Only a1) (Only a2) = Only(a1 <> a2)

instance (Monoid a) => Monoid (Optional a) where
    mempty = Nada

-- Only (Sum 1) `mappend` Only (Sum 1)
-- Only (Product 4) `mappend` Only (Product 2)
-- Only (Sum 1) `mappend` Nada
-- Only [1] `mappend` Nada


data NonEmptyList a = Elem a (NonEmptyList a) | Init a deriving (Show)

append :: a -> NonEmptyList a -> NonEmptyList a
append a (Init b) = (Elem b (Init a))
append a (Elem b rest) = Elem b (append a rest)

instance Semigroup a => Semigroup (NonEmptyList a) where
  (<>) (Init a1) (Init a2) = Elem a1 (Init a2)
  (<>) nel1 (Init a2) = append a2 nel1
  (<>) (Elem a1 r1) r2 = Elem a1 (r1 <> r2)
  (<>) (Init a1) r2 = Elem a1 r2

  -- no! breaks laws!
  -- mappend mempty (Init "1") =/= Init "1"
instance (Monoid a) => Monoid (NonEmptyList a) where
  mempty = Init (mempty a)


a = Elem "c" (Elem "b" (Init "a"))
b = Elem "9" (Elem "8" (Init "7"))