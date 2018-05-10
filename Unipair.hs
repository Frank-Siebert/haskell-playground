import Control.Applicative (liftA2)
import Control.Monad.Trans.Class

data Unipair a = Unipair a a deriving (Eq,Show,Read,Ord)
type NowThen = Unipair

instance Functor Unipair where
   fmap f (Unipair x y) = Unipair (f x) (f y)
instance Applicative Unipair where
   pure x = Unipair x x
   Unipair f f' <*> Unipair x x' = Unipair (f x) (f' x')
instance Monad Unipair where
   return = pure
   Unipair x x' >>= f = let Unipair y _ = f x
                            Unipair _ z = f x'
                         in Unipair y z

instance Foldable Unipair where
    foldMap f (Unipair x y) = f x `mappend` f y

instance Traversable Unipair where
    traverse f (Unipair x y) = Unipair <$> f x <*> f y

newtype UnipairT m a = UnipairT { runUnipair :: m (Unipair a) }

instance (Functor m) => Functor (UnipairT m) where
    fmap f = UnipairT . (fmap . fmap) f . runUnipair
instance (Applicative f) => Applicative (UnipairT f) where
    pure = UnipairT . pure . pure
    UnipairT fs <*> UnipairT xs = UnipairT (fs <<*>> xs) where (<<*>>) = liftA2 (<*>)
instance (Monad m) => Monad (UnipairT m) where
    return = pure
    UnipairT p >>= f = UnipairT $ do Unipair x y <- p
                                     let UnipairT fx = f x
                                     let UnipairT fy = f y
                                     Unipair x' _ <- fx
                                     Unipair _ y' <- fy
                                     return (Unipair x' y')
instance MonadTrans UnipairT where
    lift = UnipairT . fmap pure

data PoE a = Empty | Pair a a
instance Functor PoE where
   fmap _ Empty = Empty
   fmap f (Pair x y) = Pair (f x) (f y)
instance Applicative PoE where
   pure x = Pair x x
   b1 <*> b2 = case (b1, b2) of
     (Pair f f', Pair x x') -> Pair (f x) (f' x')
     Empty                  -> Empty
instance Monad PoE where
   Empty >>= _ = Empty
   Pair x y >>= f = case (f x, f y) of (Pair x' _,Pair _ y') -> Pair x' y' ; _ -> Empty

