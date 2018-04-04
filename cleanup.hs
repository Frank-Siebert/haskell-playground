{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

-- various bits that I littered in other files.

-- taken from wargame.hs. Mostly superseded by Unipair.
data Timed a = Timed {curr :: a, prev :: a} deriving (Functor, Foldable)

type Predicate a = a -> Bool

type TimeStrat = Bool -> Bool -> Bool

is,is',is'' :: TimeStrat -> Predicate a -> Timed a -> Bool
is ts pred timed = ts (pred . curr $ timed) (pred . prev $ timed)
is' ts pred = (\(Timed x y) -> ts x y) . fmap pred
is'' ts pred = foldr1 ts . fmap pred

now :: TimeStrat
now a _ = a

before :: TimeStrat
before _ b = b

enter :: TimeStrat
enter a b = a && not b

media = Timed 10 1
loading = (==) 10
--is enter loading media

-- taken from TimeVar.hs
data Refs s a = Refs (s -> (s,a))

data Ref s a = Ref (s -> a)

newRef :: a -> Refs s (Ref s a)
newRef x = Refs $ \s -> (s,(Ref $ \s -> x))
getRef :: Ref s a -> Refs s a
getRef (Ref sf) = Refs $ \s -> (s, sf s)
setRef :: Ref s a -> a -> Refs s ()
setRef (Ref sf) v = Refs $ \s -> (s,())
