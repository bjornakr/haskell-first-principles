module Monadix.Testing where

e0 = ((Just 1) *> (Just 2)) == Just 2
e1 = (Nothing *> (Just 2)) == Nothing
e2 = ((Just 2) *> (Nothing :: Maybe Int)) == Nothing
-- e2 = ((Just 2) *> Nothing) == Nothing -- doesn't work ???
e3 = ((Just 1) <* (Just 2)) == Just 1
