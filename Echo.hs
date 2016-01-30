main = do input <- getLine
          putStrLn input

class MyMonad m where
   myReturn :: a -> m a
   myBind :: m a -> (a -> m b) -> m b