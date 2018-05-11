--import Data.Bifunctor.Bifunctor
import Data.Functor.Constant
import Control.Applicative (liftA2)

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

{-
   Applicative Laws:
(<*>) = liftA2 id

liftA2 f x y = f <$> x <*> y

Further, any definition must satisfy the following:

identity
    pure id <*> x = x

composition
    pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
    pure (.) <*> g <*> f <*> x = g <*> (f <*> x)
                 (g . f) x  = g ( f x )

homomorphism
    pure f <*> pure x = pure (f x)

interchange
    f <*> pure x = pure ($ x) <*> f
-}
-- fmap id = id ; id <$> x = x
-- fmap g . fmap f = fmap (g . f) ; f <$> g <$> x = f . g <$> x
infixr 8 <.>
class Funcat f => Kleislicative f where
   (<.>) :: f (b->c) -> f (a->b) -> f (a->c)
   pureF :: (a->b) -> f (a->b)
{-
Applicatives are Functors, because
   pure f <*> xx = fmap f xx
The same or similar thing must be shown for Kleislicatives first.
But Functors have values, not functions. I investigated Profunctors, but this was a dead end.
So, Functor + Category, that is a Funcat:
-}
class Funcat f where
  (.>) :: (a -> b) -> f (c -> a) -> f (c -> b)
  defuncat :: f (() -> a) -> f a
  enfuncat :: f a -> f (() -> a)
infixr 5 .>

instance Funcat [] where
  f .> xs = [ f . x | x <-xs]
  enfuncat xs = map const xs
  defuncat xs = map ($()) xs
instance Kleislicative [] where
  pureF f = [f]
  gs <.> fs = [g . f | g<-gs, f<-fs]

newtype KA f a = KA { deKA :: f a }
instance Funcat f => Functor (KA f) where
   fmap f = KA . defuncat . (f .>) . enfuncat . deKA
instance Kleislicative f => Applicative (KA f) where
   pure = KA . defuncat . pureF . (\x () -> x)
   KA a <*> KA b = KA . defuncat $ a <.> enfuncat b
newtype AK f a = AK { deAK :: f a }
instance Functor f => Funcat (AK f) where
   (.>) f   = AK . fmap (f.) . deAK
   enfuncat = AK . fmap (\x () -> x) . deAK
   defuncat = AK . fmap (\x -> x ()) . deAK
instance Applicative f => Kleislicative (AK f) where
   pureF = AK . pure
   AK g <.> AK f = AK (pure (.) <*> g <*> f)
{-
Laws for Funcat:
   id .> x = x
   g .> (f .> x) = (g . f) .> x
   g .> f .> x = g . f .> x

So, what is the analogon to
   pure f <*> xx = fmap f xx
   pure f <*> xx = f <$> xx ?
it is simply
   pureF f <.> g = f .> g

So now,
a <.> b = pure (.) <*> a <*> b and
(a <*> b) = defuncat ( a <.> enfuncat b )
          = defuncat ( pure (.) <*> a <*> enfuncat b)
          = pure (\x -> x ()) <*> ( pure (.) <*> a <*> (pure (\x _ -> x) <*> b))

enfuncat xs = (\x _  -> x) <$> xs
defuncat xs = (\x -> x ()) <$> xs

    u <*> pure y = pure ($ y) <*> u
    defuncat (u <.> enfuncat (pure y))
    defuncat (u <.> pureF (\()->y))

    defuncat (pureF (\f -> f y) <.> enfuncat u)
    pure ($ y) <*> u

Simpler: ($x) f = f x
          (\h -> h x) f
if x should be a function, too, i.e  ( x = g ()) ==> g () = x, g = (\() -> x)
         (\h -> h (g ()) f
       = (\h -> (h . g) ()) f
       = ((\x -> x ()) . (\x () -> x)) ((\h -> (h . g) ()) f)
       = (\x -> x ())  ((\x () -> x) (\h -> (h . g) ()) f)
       = (\x () -> x) (\h -> (h . g) ()) f ()
       = (\h -> (h . g)) f ()
       -- the equality in question
       = (f . g) ()
       = f (g ())

So, is there a law for (.) like this: (f . g) = (.g) f? Yes it is!
                                       f . (\() -> x) = (.(\() -> x) f
(.g) f is not in <.> form. Let  (j ()) = f
    (f . g) = (.g) f
    ((j ()) . g) = (.g) (j ())
    ((j ()) . g) = ((.g) . j) ()
    ((\x -> x ()) . (\x () -> x)) ((j ()) . g) = ((.g) . j) ()
    (\x -> x ()) ( (\x () -> x) ((j ()) . g)) = ((.g) . j) ()
    ((\x () -> x) ((j ()) . g)) () = ((.g) . j) ()
    ((\x () -> x) ((j ()) . g))    = ((.g) . j)

    ((j ()) . g) = ((.g) . j) ()
    ((j .) . g) () = ((.g) . j) ()
    ((j .) . g)    = ((.g) . j)
    ((\h -> j . h) . g)    = ((\h -> h . g) . j)


    id = (\x -> x ()) . (\x () -> x) :: a -> a
    but  (\x () -> x) . (\x -> x ()) :: (() -> b) -> () -> b

a <.> b = pure (.) <*> a <*> b

reverse direction:
a <*> b = ($()) <$> (a <.> fmap const b)
or better:
a <*> b = (\x -> x ()) <$> (a <.> ((\x () -> x) <$> b))


c <*> (b <*> a) =
(\x -> x ()) <$> (c <.> ((\x () -> x) <$> (b <*> a) )) =
(\x -> x ()) <$> (c <.> ((\x () -> x) <$> ((\x -> x ()) <$> (b <.> ((\x () -> x) <$> a))) )) =
(\x -> x ()) <$> (c <.> (b <.> ((\x () -> x) <$> a)) ) =
(\x -> x ()) <$> ((c <.> b) <.> ((\x () -> x) <$> a) ) =
(c <.> b) <*> a =
(pure (.) <*> c <*> b) <*> a =
pure (.) <*> c <*> b <*>

interchange
    u <*> pure y = pure ($ y) <*> u
    -- cannot prove this directly, result is value, not function!
u <*> pure y =
(\x -> x ()) <$> (u <.> ((\x () -> x) <$> pure y)) =

(\x -> x ()) <$> (pureF (\x -> x y)  <.> ((\x () -> x) <$> u)) =
pure (\x -> x y) <*> u =
pure ($ y) <*> u
-}

class (Functor f) => Monoidal f where
   unit :: f ()
   mult :: (f a,f b) -> f (a,b)
{-
    [naturality] fmap (f *** g) (u ** v) = fmap f u ** fmap g v
 [left identity] unit ** v =~= v
[right identity] u ** unit =~= u
 [associativity] u ** (v ** w) =~= (u ** v) ** w
   where f *** g = \(x,y) -> (f x, g y)
-}
{-
mult (fmap f u , fmap g v) =
(,) <$> fmap f u <*> fmap g v
fmap (,) (fmap f u) <*> fmap g v
fmap (\a b->(a,b)) (fmap f u) <*> fmap g v
fmap ((\a b->(f a,b)) ) u <*> fmap g v

fmap (\(x,y) -> (f x, g y)) mult(u,v) =
fmap (\(x,y) -> (f x, g y)) (((,) <$> u) <*> v) =
fmap (\(x,y) -> (f x, g y)) (fmap (,) u <*> v) = -- by which law?
(\x y -> (f x, g y)) <$> u <*> v) =

fmap g ( (fmap f x) <*> yy ) == -- why?
fmap (g . f) x <*> yy

(fmap g . fmap f) xs = fmap (g . f) xs
-}
-- auf was für Sachen schränken wir uns ein wenn gilt
-- fmap fst (mult (a,b)) = a? dann war b pure!
-- fmap unit
-- pure x = fmap (const x) unit
-- f <*> x = fmap (\(a,b)-> a b) (mult f x)
--                  uncurry ($)

-- unit = pure ()
-- mult (x,y) = (,) <$> x <*> y

newtype AM f a = AM { unAM :: f a}

instance (Functor f) => Functor (AM f) where
   fmap f = AM . fmap f . unAM

instance (Applicative f) => Monoidal (AM f) where
   unit = AM (pure ())
   mult (AM x, AM y) = AM $ (,) <$> x <*> y

newtype MA f a = MA { unMA :: f a }

instance (Functor f) => Functor (MA f) where
   fmap f = MA . fmap f . unMA

instance (Monoidal f) => Applicative (MA f) where
   pure x = MA $ (\()->x) <$> unit
   MA ff <*> MA fx = MA $ fmap (\(f,x) -> f x) (mult (ff,fx))

bfib :: ([a] -> a) -> [a] -> [a]
bfib f xs = let
                x = f xs
             in x : bfib f (x:xs)

class MyKleisli m where
    (>==>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
    retur :: (a -> m a)
{-
   f >==> (g >==> h) = (f >==> g) >==> h
   f >==> retur = f = retur >==>

   does this imply a monad with
   m >= return = m
   return x >>= f = f x
   (x >>= f ) >>= g = x >>= (\y -> f y >>= g)

   f >==> g =  \x -> (f x >>= g)
   (f >==> g) x = f x >>= g
   x >>= f = ((\() -> x ) >==> f) ()

   ...
   = (x >>= f ) >>= g
   = (((\() -> x ) >==> f) ()) >>= g
   = ((\() -> ((\() -> x ) >==> f) ()) >==> g) ()
   =          ((\() -> x ) >==> f)     >==> g) ()
   =           (\() -> x ) >==> (f     >==> g) ()
   = (\() -> x ) >==> (\y -> (f     >==> g) y) ()

   = ((\One -> x ) >==> (\y -> ((\One -> f y) >==> g) One)) One
   = ((\() -> x ) >==> (\y -> f y >>= g)) ()
   = x >>= (\y -> f y >>= g)

    (f >==> g) y ==  (\() -> f y  >==> g) () ???
    (f >==> g) y == ((\() -> f y) >==> g) () !!! TODO: show! functor?
    (f `op` g) y == ((\() -> f y) `op` g) () ???
    op f g y == op (\() -> f y)  g () Nope!

   (>$) :: f (a->b) -> a -> f b
   fconst :: f b -> f (a -> b)

   (g <.> f) >$ x == (g <.> fconst (f >$ x)) >$ ()
   (g <.> f) >$ x  ==   defuncat (g <.> enfuncat (f >$ x))

   interchange again:
   f <*> pure y =
   defuncat (f <.> enfuncat (pure y))
   defuncat (f <.> pureF (\() ->y))
   f . g where g () = y
   f' () . g

   h . f' where h z = z y, f' () = f
   defuncat (pureF ($y) <.> enfuncat f)
   defuncat ((defuncat . pureF) ((\h () -> h y)) <.> enfuncat f)
   defuncat ((defuncat . pureF) ((\x () -> x) (\h -> h y)) <.> enfuncat f)
   defuncat ((defuncat . pureF . (\x () -> x)) (\h -> h y) <.> enfuncat f)
   pure (\h -> h y) <*> f

   f . g = g (f.)?
      pure = defuncat . pureF . (\x () -> x)

   attempts for defuncat enfuncat laws, well these include pure(F)
   enfuncat (pure  y) == pureF (\() ->y)
   defuncat (pureF f) == pure  (f ())

   f . (\() -> y)  == ($y) . (\x () -> x) f
-}
(>$) :: f (a->b) -> a -> f b
(>$) = undefined
