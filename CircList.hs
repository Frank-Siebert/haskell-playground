module Data.CircList
  ( fromList
  , toList
  , head
  , take
  , advance
  , replace
  , isWrap
  ) where

--
data CircList a = CircL ([a] -> [a]) [a]

normalize :: CircList a -> CircList a
normalize (CircList f []) = CircList id (f [])

fromList :: [a] -> CircList a
fromList xs = CircL id xs

head :: CircList a
head (CircList _ (x:_)) = x