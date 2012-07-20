-- Module fsFirst (myFak)

import List (sortBy)
import FsTree
import Control.Monad (liftM2)
import Data.Function (on)

myFak :: Num a => a -> a
myFak 0 = 1
myFak n = n * myFak (n-1)


applyMultiple :: Num n => 
  n -> (a -> a) -> a -> a
applyMultiple 0 _ = id
applyMultiple n f = f . (applyMultiple (n-1) f)

--(:=:) :: (a->b) -> (b->c) -> (a -> c)
--f :=: g = g . f

data Field = X | O | E
  deriving (Show,Eq)


fromField :: Field -> Int
fromField X = 1
fromField O = -1
fromField E = 0

nextPlayer :: Field -> Field
nextPlayer E = error $ "No next player after E; E ain't a player!"
nextPlayer X = O
nextPlayer O = X


type Line = (Field,Field,Field) 
type TicTacToe = (Line,Line,Line) -- TODO: wrong representation! Needs 16 cases for win!

eval :: TicTacToe -> Int
-- undo! eval ((X,X,X),(_,_,_),(_,_,_)) = 1
--eval ((a1,b1,c1),(a2,b2,c2),(a3,b3,c3)) = 0
eval _ = 0

class Position pos where
  win :: pos -> Int
  moves :: pos -> [pos]
  
data TTT = TTT TicTacToe Field -- position and player (damn E!)
  deriving Show

emptyTTT = let el = (E,E,E) in
  TTT (el,el,el) X
  
instance Position TTT where
  win (TTT line _) = 0
  moves all@(TTT line p) = 
		case line of
		  ((E ,b3,c3),(a2,b2,c2),(a1,b1,c1)) -> [TTT ((p ,b3,c3),(a2,b2,c2),(a1,b1,c1)) (nextPlayer p)]
		  otherwise -> []
	++
		case line of
		  ((a3,E ,c3),(a2,b2,c2),(a1,b1,c1)) -> [TTT ((a3,p ,c3),(a2,b2,c2),(a1,b1,c1)) (nextPlayer p)]
		  otherwise -> []
		
	  
	  
{-solve :: (a -> [a]) -> (a -> Int) -> a -> [a]
solve moves eval pos = let followUps = (map (solve moves eval) (sortBy sortcmp (moves pos))) in
    if followUps == []
	  then []
	  else pos : 
  where
    sortcmp a b = (eval a) `compare` (eval b)-}

-- TODO: define fold in terms of map and vice versa?!

fuck = (\x y -> sum (x:y))
joy = (sum .) . (:)

--sinPl :: Float -> Float -> Float
--sinPl = sin . (+)

tw :: (Num t)=>(t->t)->t
tw h = h 42

gameTree :: TTT -> Tree TTT
gameTree ttt = repTree moves ttt

stringsFromDigit :: Integer -> [String]
stringsFromDigit = map (:[]) . charsFromDigit where
    charsFromDigit 1 = " "
    charsFromDigit 2 = "abc"
    charsFromDigit 3 = "def"
    charsFromDigit 4 = "ghi"
    charsFromDigit 5 = "jkl"
    charsFromDigit 6 = "mno"
    charsFromDigit 7 = "pqrs"
    charsFromDigit 8 = "tuv"
    charsFromDigit 9 = "wxyz"
    charsFromDigit 0 = "+"
    charsFromDigit _ = error "not a digit! 0<=d<=9"

t9words :: [Integer] -> [String]
--t9words = foldr1 (liftM2 (++)) . map (map (:[])) . map stringsFromDigit 
t9words = foldr1 (liftM2 (++)) . map stringsFromDigit
t9words2 = foldr (liftM2 (++) . stringsFromDigit) [""] -- can it be made even shorter?

listFromNum :: (Integral a) => a -> a -> [a]
listFromNum = (reverse .) . listFromNum' where
    listFromNum' _    0 = []
    listFromNum' base x =  (x `mod` base) : listFromNum' base (x `div` base)

base10digits = listFromNum 10

   