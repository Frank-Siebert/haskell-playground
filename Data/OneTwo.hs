{-# LANGUAGE DeriveFunctor #-}
module Data.OneTwo
  ( OneTwo(..)
  , left
  , right
  , oneTwo
  ) where

-- goal of this experiment was to show / check if OneTwo is a Monad and under which functions.
data OneTwo a = One a | Two a a deriving (Show,Read,Eq,Functor)

instance Foldable OneTwo where
   foldr f i (One x) = f x i
   foldr f i (Two x y) = f x . f y $ i
instance Traversable OneTwo where
   traverse f (One x) = One <$> f x
   traverse f (Two x y) = Two <$> f x <*> f y
instance Applicative OneTwo where
   pure = One
   One f <*> One x = One (f x)
   otf   <*> otx   = Two (left otf $ left otx) (right otf $ right otx)
instance Monad OneTwo where
   One x >>= f = f x
   Two x y >>= f = Two (left (f x)) (right (f y))

left,right:: OneTwo a -> a
left (One x) = x
left (Two x _) = x
right (One x) = x
right (Two _ x) =x

oneTwo :: (a -> b) -> (a -> a -> b) -> OneTwo a -> b
oneTwo f _ (One x)   = f x
oneTwo _ g (Two x y) = g x y
