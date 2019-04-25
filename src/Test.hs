module Test where

data Thing = Get | Insert deriving (Eq, Show)

class Monad m => Repo m where
    get :: m Thing
    insert :: m Thing

instance Repo Maybe where
    get = Just Get
    insert = Just Insert

instance Repo IO where
    get = do
        putStrLn "Getting Thing"
        pure Get
    insert = do
        putStrLn "Inserting thing"
        pure Insert

        
-- data MonadRepo m where
--     MonadRepo :: (Monad m, Repo m) => MonadRepo m

app :: (Monad m, Repo m) => m [Thing]
app = do
    g1 <- get
    g2 <- get
    i1 <- insert
    pure [g1, g2, i1]

main :: IO ()
main = do
    putStrLn "app with fake"
    let appFake = app :: Maybe [Thing]
    putStrLn $ show appFake
    
    putStrLn "app with real"
    appReal <- app :: IO [Thing]
    putStrLn $ show appReal
    pure ()
