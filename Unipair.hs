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
