-- tying the knot for fun

import Data.Tree

data D a = D1 (D a) a (D a) | End


fromList :: [a] -> D a
fromList [] = End
fromList  xs = go End xs where
               go :: D a -> [a] -> D a
               go left [] = End
               go left (x:xs) = let this = D1 left x (go this xs) in this

data MyTree a = MyTree a (Maybe (MyTree a)) [MyTree a]

fromTree :: Tree a -> MyTree a
fromTree = go Nothing where
           go :: Maybe (MyTree a) -> Tree a -> MyTree a
           go parent (Node x subs) = let this = MyTree x parent (map (go (Just this)) subs) in this
