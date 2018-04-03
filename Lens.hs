{-# LANGUAGE Rank2Types #-}
-- my lens playground
import Control.Monad.State
import Data.Functor.Constant
import Data.Functor.Identity

type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)
originalLensCompose :: Lens s t a b -> Lens a b c d -> Lens s t c d
originalLensCompose = (.)

originalLensSet :: Lens s t a b -> b -> s -> t
--iginalLensSet f v h = runIdentity $ f (\_ -> Identity v) h
originalLensSet f v = runIdentity . f ( const $ Identity v)

originalLensGet :: Lens s t a b -> s -> a
--iginalLensGet f h = getConstant $ f (Constant) h
originalLensGet f = getConstant . f Constant

originalLensMod :: Lens s t a b -> (a->b) -> s -> t
originalLensMod f m = runIdentity . f (fmap m . Identity)

data MyData a b = MyData a b deriving (Eq,Show)
arr :: MyLens (MyData a b) a
bee :: MyLens (MyData a b) b
arr = MyLens (\(MyData a _) -> a) (\v (MyData _ b) -> MyData v b)
bee = MyLens (\(MyData _ b) -> b) (\v (MyData a _) -> MyData a v)

-- now have a original Lens for MyData!
bol :: Lens(MyData a b) (MyData a c) b c
--bol :: Functor f => (b -> f c) -> MyData a b -> f (MyData a c)
bol f (MyData x y) = MyData x <$> f y
aol f (MyData x y) = flip MyData y <$> f x

type Lens' s a = Lens s s a a

data MyLens host val = MyLens (host -> val) (val -> host -> host)

flat :: MyLens a a
flat = MyLens id const

(.^) :: host -> MyLens host val -> val
h .^ (MyLens r _) = r h
infixl 1 .^

set :: MyLens h v -> v -> h -> h
set (MyLens _ s) = s

myRead :: MyLens (MyLens h v) (h -> v)
myRead = MyLens (\(MyLens g _) -> g) (\v (MyLens _ s) -> MyLens v s)

myWrite :: MyLens (MyLens h v) (v -> h -> h)
myWrite = MyLens (\(MyLens _ s) -> s) (\v (MyLens g _) -> MyLens g v)

with :: MyLens h v -> State v a -> State h a
with (MyLens g s) m = do h <- get
                         let (a,v') = runState m (g h)
                         put (s v' h)
                         return a
-- with is called zoom in some official lens. What a beautiful naming!
-- official zoom is much more general than this.
zoom :: Lens s s a a -> State a r -> State s r
zoom l m = do s <- get
              let (r,a') = runState m (originalLensGet l s)
              put (originalLensSet l a' s)
              return r

someAction :: State (MyData Int String) ()
someAction = do with arr $ do modify (+1)
                with bee $ do z <- get
                              put (z++z)

sample = MyData 0 "x"
rsa = runState someAction sample

mod :: h -> MyLens h v -> (v -> v) -> h
mod h (MyLens g s) f =  s (f (g h)) h


(.:) :: MyLens a b -> MyLens b c -> MyLens a c
(MyLens g1 s1) .: (MyLens g2 s2) = MyLens (g2 . g1) (\v h -> s1 (s2 v (g1 h)) h)

data MLens h v = MLens (h -> v) ((v -> v) -> h -> h)
{-
      (h -> v , (v->v)->h->h)
      (h -> v , h -> (v -> v) -> h)
      h -> (v, (v-> v) -> h)
-}

composeMLens :: MLens a b -> MLens b c -> MLens a c
composeMLens (MLens g1 m1) (MLens g2 m2) = MLens (g2 . g1) (\f h -> m1 (m2 f) h )

data Lens4 s t a b = Lens4 (s -> a) ((a -> b) -> s -> t)
composeLens4 :: Lens4 s t a b -> Lens4 a b c d -> Lens4 s t c d
--composeLens4 (Lens4 g1 m1) (Lens4 g2 m2) =  Lens4 (g2 . g1) (\f h -> m1 (m2 f) h)
composeLens4 (Lens4 g1 m1) (Lens4 g2 m2) =  Lens4 (g2 . g1) (m1 . m2)

data Lens5 s t a b = Lens5 (t -> b) ((a -> b) -> s -> t)
composeLens5 :: Lens5 s t a b -> Lens5 a b c d -> Lens5 s t c d
composeLens5 (Lens5 g1 m1) (Lens5 g2 m2) =  Lens5 (g2 . g1) (m1 . m2)

mmd = MyData (MyData 'a' "b") (MyData (1::Int) ( 2.0 :: Double))