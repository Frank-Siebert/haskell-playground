module FsTree where

data Tree payload = Tree payload [Tree payload]
  --deriving Show

instance (Show a) => Show (Tree a) where
  show = show' ""  where
    show' s (Tree x sub) = s ++ (show x) ++ "\n" ++ (foldr (++) "" (map (show' (s++"    ")) sub))

--TODO: Tree is an instance of Functor, making fmap = mapTree
  
takeTree :: Int -> Tree a -> Tree a
takeTree 0 (Tree x  _ ) = Tree x []
takeTree n (Tree x sub) = Tree x (map (takeTree (n-1)) sub)

repTree :: (a -> [a]) -> a -> Tree a
repTree f a = Tree a (map (repTree f) (f a))

foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f (Tree payload subtrees) = f payload [ foldTree f subtree | subtree <- subtrees]

  
mapTree :: (a->b) -> Tree a -> Tree b
mapTree f (Tree x sub) = Tree (f x) (map (mapTree f) sub)

mapTree2 :: (a->a) -> (a->a) -> Tree a -> Tree a
mapTree2 fcum f (Tree x subs) = (Tree (fcum x) (map (mapTree2 (fcum . f) f) subs))


myTree = (Tree 5 [(Tree 3 [ Tree 2 [] , (Tree 4 [])]), Tree 7 []]) -- :: Tree Integer

height :: Tree a -> Int
-- does not work -- height = foldTree (\_ x -> 1 + max (0:x))
height = foldTree detHeight where
  detHeight _ sub = 1 + maximum (0:sub)
  
lengthTree :: Tree a -> Int
lengthTree = foldTree detLength where
  detLength _ sub = 1 + sum sub

leaves :: Tree a -> Int
leaves = foldTree detLeafCount where
  detLeafCount _ [] = 1;
  detLeafCount _ sub = sum sub
