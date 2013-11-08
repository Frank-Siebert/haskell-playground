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
data CircList a = CircL ([a] -> [a]) a [a]

-- difflists cannot be functors!
--instance Functor CircList where
--  fmap func (CircL f hd xs) = CircL (map func . f) (func hd) (map func xs)

mkCircList :: a -> [a] -> CircList a
mkCircList x xs = CircL id x xs

head :: CircList a -> a
head (CircL _ x _) = x

pop :: CircList a -> CircList a
pop (CircL f hd (x:xs)) = CircL f x xs

