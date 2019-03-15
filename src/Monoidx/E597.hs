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
