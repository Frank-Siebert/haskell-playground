module Hashable where

import Data.Array
import Data.Char (ord)
import Data.Int (Int32)
import Data.List (tails)
import Maybe (listToMaybe,fromJust)


class (Eq a) => Hashable a where
   hash :: a -> Int32
   
instance Hashable Char where
   hash = fromInteger . toInteger . ord

instance Hashable Int where
   hash = fromInteger . toInteger

instance (Hashable a) => Hashable [a] where
   hash [] = 0 -- isn't this a fold?
   hash (a:as) = hash a + 257 * (hash as)
   
instance (Hashable a, Hashable b) => Hashable (a,b) where
   hash (a,b) = hash a + 7 * hash b
   
type HashMap a b = Array Int32 [(a,b)]

newHashMap :: (Hashable a) => Int32 -> HashMap a b 
newHashMap n = array (0,n-1) [(i,[]) | i <- [0..n-1]]

insert :: (Hashable a) => HashMap a b -> (a , b) -> HashMap a b
insert arr kvp@(k,_) = let h = (hash k) `mod` (fromInteger . toInteger . rangeSize . bounds $ arr) in arr // [(h, kvp:arr!h)]

lookUp :: (Hashable a) => HashMap a b -> a -> Maybe b
lookUp hm a = let h = (hash a) `mod` (fromInteger . toInteger . rangeSize . bounds $ hm) in listToMaybe [ v | (k,v) <- hm ! h, a==k]

get :: (Hashable a) => HashMap a b -> a -> b
get hm a = fromJust (lookUp hm a)
--get = fromJust . lookUp
