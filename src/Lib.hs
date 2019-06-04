module Lib
    ( someFunc
    ) where

import Control.Applicative

someFunc :: IO ()
someFunc = putStrLn "someFunc"

ap :: [(a -> b)] -> [a] -> [b]
ap [] as = []
ap _ [] = []
ap (f:fs) (as) = fmap f as ++ ap fs as



deckOfCards = ((,) <$> ["S", "H", "D", "C"]) <*> ((show <$> [1..10]) <> ["J", "Q", "K", "A"])

anotherDeckOfCards = liftA2 (,) ["S", "H", "D", "C"] ((show <$> [1..10]) <> ["J", "Q", "K", "A"])
