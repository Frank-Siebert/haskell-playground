module Data.CircList
  ( CircList
  , mkCircList
  , getHead
  , getLeft
  , getRight
  , reverseC
  , toFiniteList
  , toInfList
  , rewind
  , left
  , right
  , replace
  ) where
  
import Prelude hiding (head)
import Control.Comonad

--
data CircList a = C { getLeft :: [a], getHead :: a, getRight :: [a] }
-- internally normalized: rs==[] implies ls==[]
-- this normalization is bullshit. It is fine if there is no central element.

-- difflists cannot be functors!
instance Functor CircList where
  fmap f (C ls h rs) = C (map f ls) (f h) (map f rs)
  
instance Comonad CircList where
  extract = getHead
  duplicate c = C (lefts c) c (rights c)
  
instance (Show a) => Show (CircList a) where
  show (C ls h rs) = show (reverse ls) ++ show h ++ show rs

mkCircList :: [a] -> a -> [a] -> CircList a
mkCircList ls x xs = C ls x xs

reverseC :: CircList a -> CircList a
reverseC (C ls h rs) = C rs h ls

toFiniteList :: CircList a -> [a]
toFiniteList (C ls h rs) = (h:rs) ++ reverse ls

toInfList :: CircList a -> [a]
toInfList = cycle . toFiniteList

left :: CircList a -> CircList a
left cl@(C [] _ []) = cl
left    (C [] h rs) = let (x:xs) = reverse (h:rs) in C (xs) x []
left    (C (l:ls) h rs) = C ls l (h:rs)

right :: CircList a -> CircList a
right cl@(C [] _ []) = cl
right    (C ls h []) = let (x:xs) = reverse (h:ls) in C [] x xs 
right    (C ls h (r:rs)) = C (h:ls) r rs

lefts :: CircList a -> [CircList a]
lefts (C [] _ _) = []
lefts c = let l = left c in l:lefts l

rights :: CircList a -> [CircList a]
rights (C _ _ []) = []
rights c = let r = right c in r:rights r

replace :: a -> CircList a -> CircList a
replace x (C ls _ rs) = C ls x rs

rewind :: CircList a -> CircList a
rewind cl@(C [] _ _) = cl
rewind (C (l:ls) h rs) = rewind (C ls l (h:rs))

dual :: (CircList a -> CircList a) -> (CircList a -> CircList a)
dual f = reverseC . f . reverseC
