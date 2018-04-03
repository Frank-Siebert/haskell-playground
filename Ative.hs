import Data.Bifunctor.Bifunctor
import Data.Functor.Constant

-- plays with associative (which is rather useless) and the equivalence
-- of Applicative with (f a, f b) -> f (a,b)

class Associative a where
   assoRL :: a x (a y z) -> a (a x y) z
   assoLR :: a (a x y) z -> a x (a y z)

instance Associative (,) where
   assoRL (x,(y,z)) = ((x,y),z)
   assoLR ((x,y),z) = (x,(y,z))

instance Associative Either where
   assoRL (Left  x)         = Left  (Left  x)
   assoRL (Right (Left  y)) = Left  (Right y)
   assoRL (Right (Right z)) = Right z
   assoLR (Left  (Left  x)) = Left  x
   assoLR (Left  (Right y)) = Right (Left  y)
   assoLR (Right z)         = Right (Right z)

instance Associative Constant where
   assoRL (Constant x) = Constant (Constant x)
   assoLR (Constant (Constant x)) = Constant x

--instance Bifunctor f => Associative -- no, Commutative, not asso

class (Functor f) => Appli f where
   unit :: f ()
   mult :: (f a,f b) -> f (a,b)

-- auf was für Sachen schränken wir uns ein wenn gilt
-- fmap fst (mult (a,b)) = a? dann war b pure!
-- fmap unit
-- pure x = fmap (const x) unit
-- f <*> x = fmap (\(a,b)-> a b) (mult f x)
--                  uncurry ($)

-- unit = pure ()
-- mult (x,y) = (,) <$> x <*> y

newtype Wrappli f a = Wrappli { unWrappli :: f a}

instance (Functor f) => Functor (Wrappli f) where
   fmap f = Wrappli . fmap f . unWrappli

instance (Applicative f) => Appli (Wrappli f) where
   unit = Wrappli (pure ())
   mult (Wrappli x, Wrappli y) = Wrappli $ (,) <$> x <*> y

newtype Wrapplicative f a = Wrapplicative { unWrapplicative :: f a }

instance (Functor f) => Functor (Wrapplicative f) where
   fmap f = Wrapplicative . fmap f . unWrapplicative

instance (Appli f) => Applicative (Wrapplicative f) where
   pure x = Wrapplicative $ const x <$> unit
   Wrapplicative ff <*> Wrapplicative fx = Wrapplicative $ fmap (\(f,x) -> f x) (mult (ff,fx))

bfib :: ([a] -> a) -> [a] -> [a]
bfib f xs = let
                x = f xs
             in x : bfib f (x:xs)