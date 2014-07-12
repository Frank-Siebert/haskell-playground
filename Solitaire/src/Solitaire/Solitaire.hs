module Solitaire.Solitaire where

import Control.Monad (guard)
import Control.Monad.Trans.State.Strict (State, evalState, modify, get)
import Data.Bits (shiftR,(.&.))
import Data.List (elemIndex)
import Data.Maybe (fromJust)


data Suit = Clubs | Diamonds | Hearts | Spades deriving (Show,Read,Eq,Enum,Bounded)
data Rank = Ace | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | Jack | Queen | King deriving (Show,Read,Eq,Enum,Ord,Bounded)
data Card = Rank :/ Suit deriving (Show,Read,Eq)

type Cards = [Card]
data Column = Column [Card] [Card] deriving (Show)-- hidden, open
type Table = [Column]
-- TODO undealt cards.

data GameState = GameState {
                     table :: Table,
                     undealt :: Cards }
                     
dealHiddenCards :: Cards -> GameState
dealHiddenCards = go 44 0 (replicate 10 []) where
               go :: Int -> Int -> [[Card]] -> Cards -> GameState
               go 0 _ cols cards = GameState { table = map (\x -> Column x []) cols, undealt = cards }
               go n colIndex cols (card:cards) = go (n-1) (colIndex+1 `rem` 10) cols cards
               
-- | deals a row of open cards. Can be used initially and in subsequent dealings.
dealOpenCards :: GameState -> GameState
dealOpenCards = go 0 where
               go 11 state = state
               go colIndex state = let (card:cards) = undealt state
                    in go (colIndex+1) (GameState { table = modList colIndex (\(Column h op)->Column h (card:op)) (table state), undealt = cards})
-- the above will be hard to understand tomorrow!

-- | opens card
normalizeColumn :: Column -> Column
normalizeColumn (Column (h:hs) []) = Column hs [h]
normalizeColumn col = col

lowestRun :: Column -> ([Card],[Card], [Card])
lowestRun (Column [] []) = ([],[],[])
lowestRun (Column hid (c:cs)) = go c cs [c] where
               go c' (c:cs) run | c' `fitsSuit` c = go c cs (c:run)
               go _  cs     run               = (hid,run,cs)
lowestRun _ = error "column not normalized"
        
fitsSuit :: Card -> Card -> Bool
fitsSuit (r1 :/ s1) (r2 :/ s2) = s1 == s2
                        && fromEnum r1 + 1 == fromEnum r2
fits :: Card -> Card -> Bool
fits (r1 :/ _) (r2 :/ _) = fromEnum r1 + 1 == fromEnum r2

fitsColumn :: Card -> Column -> Bool
fitsColumn _ (Column [] []) = True
fitsColumn c' (Column _ (c:_)) = c' `fits` c
fitsColumn _ _ = error "column not normalized"

exampleColumn = Column [] $ [x :/ Spades | x<-[Ace .. R5]] ++ [R6 :/ Hearts, King :/ Hearts, Queen :/ Diamonds]

tails':: [a] -> [[a]]
tails' = go [] where
    go accum [] = accum
    go accum x@(_:xs) = go (x:accum) xs

-- Problem: I want to return table, but I have to insert the modified column AT THE RIGHT POSITION
{-
move :: ([Card],[Card]) -> [Column] -> [Table]
move = go []  where
     go accum _ [] [] = accum
     go accum (run,cs) (col:cols) colsOnLeft = go (these++accum) (run,cs) cols (colsOnLeft ++ [col]) where
        these = [ colsOnLeft++[]+cols | x@(highestMoved:_) <- tails' run, highestMoved `fitsColumn` col]
-}

-- | reverses first list and appends
(++/) :: [a] -> [a] -> [a]
[] ++/ b = b
(a:as) ++/ b = as ++/ (a:b)

moveCol :: ([Card],[Card],[Card]) -> Column -> [(Column,Column)]
moveCol ([],[],[]) dst = []
moveCol (hid,run,cs) dstCol@(Column dsthid dst) =
  [(Column hid (drop (length subrun) (reverse run) ++ cs),Column dsthid (subrun ++/ dst)) |
    subrun <- tails' run, head subrun `fitsColumn` dstCol]

moveCols :: (Column,Column) -> [(Column,Column)]
moveCols (src,dst) = moveCol (lowestRun src) dst

{-
I need a function (I named it `applyToEveryPair`) that works on lists, e.g. `[x0,x1,x2,x3,...]`, and uses a function
`f:: (a,a) -> [(a,a)]`. For every (distinct) pair `(xi,xj)`, `i/=j` of the list
I want all lists where `xi` is replaced by `yik` and `xj` is replaced by `yjk`.

If input and output of `f` are denoted by
    f (x,y) = [(x0,y0),...,(xk,yk)]
and our sample input is
    applyToEveryPair f [a0,...,an]
    
    [a0, a1 ,a2 ,a3 ]  -- input to applyToEveryPair
      |   |
      |   |
      v   v
      f(a0, a1) =[(b0,b1),(c0,c1)]
      |   |
      v   v
    [b0, b1, a2, a3]  -- to be included in output
    [c0, c1, a2, a3]  -- to be included in output
       ...            -- more output
          |      |    -- now combine (a3,a1)
           \    /
            \  /
             \/
             /\
            /  \
         f(a3, a1) =
         [(d3, d1)]
            \  /
             \/
             /\
            /  \
           /    \
    [a0, d1 ,a2 ,d3 ]  -- to be included in output

A use case would be a matrix and compute all resulting matrices for every (pairwise) line swap,
or, in my case, all possible move in a solitaire game from one stack of cards to another.

The solution I use feels not very haskellish. I want to know if this is a good way to write it,
but especially the double use of `setList` looks terrible to me.

here is the code
-}

applyToEveryPair :: ((a,a) -> [(a,a)]) -> [a] -> [[a]]
applyToEveryPair f (xs) = do i<-[0..length xs - 1]
                             j<-[0..length xs - 1]
                             guard (i /= j)
                             (x',y') <- f (xs !! i, xs !! j)
                             return . setList i x' . setList j y' $ xs

-- | setList n x xs replaces the nth element in xs by x
setList :: Int -> a -> [a] -> [a]
setList n x = modList n (const x)

modList :: Int -> (a -> a) -> [a] -> [a]
modList _ _ [] = []
modList 0 f (x:xs) = f x : xs
modList n f (x:xs) =   x : modList (n-1) f xs

{-
Long story, short code. I think comonads are overkill here, and I have not understood Lenses (yet),
but have a vague feeling lenses apply here.
http://codereview.stackexchange.com/questions/47005/replacing-every-pair-in-a-list
-}

moves :: Table -> [Table]
moves = applyToEveryPair moveCols


-- this function is just an exercise
applyToEverySingle :: (a->a) -> [a] -> [[a]]
applyToEverySingle f [] = []
applyToEverySingle f (x:xs) = (f x :xs) : (map (x:) (applyToEverySingle f xs))

applyToEverySingle' :: (a->[a]) -> [a] -> [[a]]
applyToEverySingle' f [] = []
applyToEverySingle' f (x:xs) = (map (:xs) (f x) ) ++ (map (x:) (applyToEverySingle' f xs))

--atable :: Table
--atable = [exampleColumn, [Ace :/ Hearts], [],[R2 :/ Hearts, R3 :/ Clubs], [R3 :/ Hearts, R4 :/ Hearts, R7 :/ Hearts]]

-- (((holdrand = holdrand * 214013 + 2531011) >> 16) & 0x7fff)

randStep :: Int -> Int
randStep holdrand = holdrand * 214013 + 2531011
rand :: State Int Int
rand = do
    modify randStep
    x <- get
    return $ (x `shiftR` 16) .&. 0x7fff

getAnUndrawn :: [Int] -> State Int Int
getAnUndrawn drawn = do
    k <- rand
    let c = k `mod` 104
    if c `elem` drawn
        then getAnUndrawn drawn
        else return c

-- | In: randseed, out: list of randoms in sequence
shuffleAll :: Int -> [Int]
shuffleAll rs = reverse $ evalState (shuffleDeck []) rs
-- shuffleAll produces identical output to de.genialitaet.visualc.Random.main(String[]), so this is correct!

-- something is wrong. function is its own inverse.
unpermute :: [Int] -> [Int]
unpermute ls = [ fromJust (elemIndex i ls) | i <- [0 .. length ls - 1]]

shuffleDeck :: [Int] -> State Int [Int]
shuffleDeck cards = do
    if length cards /= 104
        then do
            k <- getAnUndrawn cards
            shuffleDeck (k:cards)
        else
            return cards

sortedDeck = let single = [r :/ s | s<-[minBound..maxBound],
                                    r<-[minBound..maxBound]] in single ++ single
-- map (sortedDeck !!) $ shuffleAll 1

tst = map (sortedDeck !!) . unpermute . shuffleAll $ 0x4603970b

