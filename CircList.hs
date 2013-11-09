module Data.CircList
  ( mkCircList
  , head
  , toFiniteList
  , toInfList
  , rewind
  , advance
  , replace
  , isWrap
  ) where
  
import Prelude hiding (head)

--
data CircList a = C [a] a [a]
-- internally normalized: rs==[] implies ls==[]

-- difflists cannot be functors!
instance Functor CircList where
  fmap f (C ls h rs) = C (map f ls) (f h) (map f rs)

mkCircList :: a -> [a] -> CircList a
mkCircList x xs = C [] x xs

head :: CircList a -> a
head (C _ x _) = x

toFiniteList :: CircList a -> [a]
toFiniteList (C ls h rs) = (h:rs) ++ reverse ls

toInfList :: CircList a -> [a]
toInfList = cycle . toFiniteList

advance :: CircList a -> CircList a
advance cl@(C [] _ []) = cl
advance    (C ls h [r]) = C [] r (reverse (h:ls))
advance    (C ls h (r:rs)) = C (h:ls) r rs
advance    _ = error "CircList is not normalized"

replace :: a -> CircList a -> CircList a
replace x (C ls _ rs) = C ls x rs

rewind :: CircList a -> CircList a
rewind cl@(C [] _ _) = cl
rewind (C (l:ls) h rs) = rewind (C ls l (h:rs))

isWrap :: CircList a -> Bool
isWrap (C [] _ _) = True
isWrap _ = False