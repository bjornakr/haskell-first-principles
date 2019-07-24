import Control.Monad (join)

-- join == flatten

bind f = join . (fmap f)

bind' f x = join . (fmap (f x))

