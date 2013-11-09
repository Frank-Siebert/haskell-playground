module Data.CircList
  ( mkCircList
  , head
  , toFiniteList
  , toInfList
  , rewind
  , left
  , right
  , replace
  , isWrap
  ) where
  
import Prelude hiding (head)
import Control.Comonad

--
data CircList a = C [a] a [a]
-- internally normalized: rs==[] implies ls==[]
-- this normalization is bullshit. It is fine if there is no central element.

-- difflists cannot be functors!
instance Functor CircList where
  fmap f (C ls h rs) = C (map f ls) (f h) (map f rs)
  
instance (Show a) => Show (CircList a) where
  show (C ls h rs) = show (reverse ls) ++ show h ++ show rs

mkCircList :: a -> [a] -> CircList a
mkCircList x xs = C [] x xs

head :: CircList a -> a
head (C _ x _) = x

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

replace :: a -> CircList a -> CircList a
replace x (C ls _ rs) = C ls x rs

rewind :: CircList a -> CircList a
rewind cl@(C [] _ _) = cl
rewind (C (l:ls) h rs) = rewind (C ls l (h:rs))

isWrap :: CircList a -> Bool
isWrap (C [] _ _) = True
isWrap _ = False