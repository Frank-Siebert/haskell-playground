module TicTacToeSolve where

import FsTree

data Player = X | O | E
  deriving (Show, Eq)

next :: Player -> Player
next E = error "No next Player for E"
next X = O
next O = X

goal :: Player -> ([Int] -> Int)
goal E = error "No goal for Player E"
goal X = maximum
goal O = minimum

type Board = [Player]
type Board2 = [[Player]]

empty :: Board
empty = replicate 9 E

startPos = Position empty X
winX = Position [E,O,E,E,X,E,E,E,E] X
winO = Position [E,X,E,E,O,E,E,X,E] O -- my fault, draw

-- zwickmühle
zwick = gameTree (Position [X,E,X,E,O,E,X,E,O] O)

to2 :: Board -> Board2
to2 [a3,b3,c3,a2,b2,c2,a1,b1,c1] = [[a3,b3,c3],[a2,b2,c2],[a1,b1,c1]]

data Position = Position Board Player
  deriving (Show)
  
instance Eq Position where
  (Position board1 p1) == (Position board2 p2) = board1 `rotEq` board2 && p1 == p2
  
rotEq :: Board -> Board -> Bool
rotEq a b = if (length a) /= 9 then a==b
              else
			    a==b || a == (reverse b) || a== (rotClk b) || rotClk a == b || mirY a == b || (mirY . rotClk $ a) == rotClk b
				     || a == mirD b || mirD (mirY a) == mirD b
				  where
				    rotClk [a3,b3,c3,a2,b2,c2,a1,b1,c1] = [a1,a2,a3,b1,b2,b3,c1,c2,c3];
					mirY [a3,b3,c3,a2,b2,c2,a1,b1,c1] = [a1,b1,c1,a2,b2,c2,a3,b3,c3];
					mirD [a3,b3,c3,a2,b2,c2,a1,b1,c1] = [a3,a2,a1,b3,b2,b1,c3,c2,c1]

rotClk :: Board2 -> Board2
rotClk [[a3,b3,c3],[a2,b2,c2],[a1,b1,c1]] = [[a1,a2,a3],[b1,b2,b3],[c1,c2,c3]]

eval :: Board -> Int
eval board = let b = to2 board in
  if evalFor b X || evalFor (rotClk b) X then 1
    else if evalFor b O || evalFor (rotClk b) O then -1
	  else 0
					
evalFor :: Board2 -> Player-> Bool
evalFor b p = any ([p,p,p] ==) b || diag b
  where diag [[a3,_,_],[_,b2,_],[_,_,c1]] =  a3 == b2 && b2 == c1 && c1 == p
  
 
  
add :: (Eq a) => a -> [a] -> [a]
x `add` [] = [x]
x `add` xs = if x `elem` xs then xs else x:xs

-- works: myMap f = foldr (\x y -> (f x):y) []

mapAdd :: (Eq b) => (a->b) -> [a] -> [b]
mapAdd f = foldr (\x y -> (f x) `add` y) []
  
moves :: Position -> [Position]
moves pos@(Position board p) = if (eval board) /= 0 then []
    else moves' pos
  where
	moves' (Position  []   p) = []
	moves' (Position (E:r) p) = Position (p:r) (next p) `add`
							   mapAdd (prepend E) (moves' (Position r p))
	moves' (Position (f:r) p) = mapAdd (prepend f) (moves' (Position r p))

prepend :: Player -> Position -> Position
prepend f (Position b p) = Position (f:b) p

gameTree :: Position -> Tree Position
gameTree pos = repTree moves pos

fullGameTree = gameTree startPos

evalTree :: Tree Position -> Tree (Int, Position)
evalTree = let neg1st (v,pos) = (-v,pos) in
  --mapTree2 neg1st neg1st . mapTree (\all@(Position board p) -> (eval board , all ))
  mapTree2 id id . mapTree (\all@(Position board p) -> (eval board , all ))
  
-- TODO: shouldn't it be called minTree?
maxTree :: Tree (Int, Position) -> Tree (Int, Position)
--maxTree = foldTree (((maximum .) (:)) `tupf2` (\x _ _ -> x))
--maxTree = mapTree (((maximum .) (:)) `tupf2` (\x _ _ -> x))
-- maxTree (Tree (x , pos) subs) = Tree (1, startPos) [] - stub is compiling
maxTree (Tree (x , pos) subs) = let subsMax = (map maxTree subs) in
  (Tree  (negate (minimum (x:(map (\(Tree (x, _) _)->x) subsMax))), pos) subsMax)
  
minMax :: Tree (Int, Position) -> Tree (Int, Position)
  Tree (val subsGoal, pos) subsGoal where
    val [] = v;
	val l  = (goal p) (map (\(Tree (x, _) _)->x) l)

-- tupel function, arity 2
tupf2 :: (a->b) -> (c->d) -> ((a,c) -> (b,d))
tupf2 f g (x,y) = (f x, g y)

endGame = gameTree (Position [X,X,O,X,E,X,O,E,O] X)


