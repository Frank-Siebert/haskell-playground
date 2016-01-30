{- # LANGUAGE StandaloneDeriving #-}
{- # LANGUAGE FlexibleContexts #-}

import Control.Applicative
import Control.Monad(join)
import Data.Foldable
import Data.Monoid
import Data.Traversable

newtype Cont r a = Cont { runCont :: ((a->r) -> r) }

instance Functor (Cont r) where
    fmap f (Cont x) = Cont $ \c -> x (c . f)

c1 :: Cont r Char
c1 = Cont $ \r -> r 'c'

instance Applicative (Cont r) where
    pure x = Cont ($x)
    Cont f <*> Cont x = Cont $ \c -> f $ \c' -> x (c . c')

instance Monad (Cont r) where
    return = pure
    Cont x >>= f = Cont $ \c -> x $ \c' -> let Cont f' = f c' in f' c

type Free f a = Cont (f a) a

pair :: Cont r a -> Cont r (a,a)
pair (Cont x) = Cont $ \c -> x (c . (\a -> (a,a)))

--mapf :: (r -> s) -> Cont r a -> Cont s a
--mapf f (Cont x) = Cont $ \c -> _ (f . x) c

run :: Cont r r -> r
run (Cont x) = x id

returnFree x = Cont ($ x)
impureFree x = undefined

data FreeMonad f a = Pure a | Impure (f (FreeMonad f a))
instance (Functor f) => Functor (FreeMonad f) where
    fmap f (Pure x)    = Pure $ f x
    fmap f (Impure xs) = Impure $ fmap (fmap f) xs
instance Functor f => Applicative (FreeMonad f) where
    pure = Pure
    Pure   f <*> x = fmap f x
    Impure f <*> x = Impure $ fmap (<*> x) f
instance Functor f => Monad (FreeMonad f) where
    return = Pure
    Pure   x >>= k = k x
    Impure x >>= k = Impure $ fmap (>>= k) x
instance (Foldable f) => Foldable (FreeMonad f) where
    foldMap f (Pure   x) = f x
    foldMap f (Impure x) = foldMap (foldMap f) x
instance (Traversable f) => Traversable (FreeMonad f) where
    traverse f (Pure   x) = Pure   <$> f x
    traverse f (Impure x) = Impure <$> traverse (traverse f) x

--deriving instance (Show a, Show (f (FreeMonad f a))) => Show (FreeMonad f a)

--data FreeFoldable a

step :: (Monad m) => FreeMonad m a -> m (Either a (FreeMonad m a))
step (Pure x) = return $ Left x
step (Impure x) = do rem <- x
                     return $ Right rem

lift :: IO x -> FreeMonad IO x
lift action = Impure $ action >>= return . Pure

echo :: FreeMonad IO ()
echo = do x <- lift $ do putStr "? "
                         getLine
          lift $ putStrLn x

yzzy x = Cont $ \fred -> fred x ++ fred x

yz f x = Cont $ \fred -> f (fred x)
--void playEntry(Entry entry, Callback<boolean> callback)
-- playEntry :: String -> (Bool -> r) -> r

--on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
--on c f x = (flip fmap f) (fmap c f x)
--on c f = flip fmap f . fmap c f

on = join <$> ((flip <$> (fmap <$>)) <$>) <$> fmap

--on' :: (Functor f1, Functor f2) => (f1 (f2 b)) -> (a -> b) -> (f1 (f2 a))
--on' x f= fmap (fmap f) x -- don't try to generalize, it is impossible?

class Contrafunctor f where
   pamf :: (a -> b) -> f b -> f a
-- don't compose, they are -1 - like: 2 make a Functor, 3 are a Contrafunctor again
--on' :: (Contrafunctor f1, Contrafunctor f2) => (f1 (f2 b)) -> (a -> b) -> (f2 (f1 a))
--on' x f = pamf (pamf f) x
on' x f = pamf (pamf (pamf f)) x

type T a b c = a b (a b c)
type F = (->)

on'' :: (T F c b) -> (a -> b) -> (T F c a)
on'' = undefined
--on'' c f x y = c (f x) (f y)
