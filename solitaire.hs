import Control.Monad (guard)

data Suit = Hearts | Spades | Diamonds | Clubs deriving (Show,Read,Eq,Enum)
data Rank = Ace | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | Jack | Queen | King deriving (Show,Read,Eq,Enum,Ord)
data Card = Rank :/ Suit deriving (Show,Read,Eq)

type Column = [Card]
type Table = [Column]
-- TODO undealt cards.

lowestRun :: Column -> ([Card], [Card])
lowestRun [] = ([],[])
lowestRun (c:cs) = go c cs [c] where
               go c' (c:cs) run | c' `fitsSuit` c = go c cs (c:run)
               go _  cs     run               = (run,cs)  
        
fitsSuit :: Card -> Card -> Bool
fitsSuit (r1 :/ s1) (r2 :/ s2) = s1 == s2
                        && fromEnum r1 + 1 == fromEnum r2
fits :: Card -> Card -> Bool
fits (r1 :/ _) (r2 :/ _) = fromEnum r1 + 1 == fromEnum r2

fitsColumn :: Card -> Column -> Bool
fitsColumn _ [] = True
fitsColumn c' (c:_) = c' `fits` c                 

exampleColumn = [x :/ Spades | x<-[Ace .. R5]] ++ [R6 :/ Hearts, King :/ Hearts, Queen :/ Diamonds]

main :: IO ()
main = do
    print $ exampleColumn
    print $ lowestRun exampleColumn

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

moveCol :: ([Card],[Card]) -> Column -> [(Column,Column)]
moveCol ([],[]) dst = [([],dst)]
moveCol (run,cs) dst = [(drop (length subrun) (reverse run) ++ cs,subrun ++/ dst) | subrun <- tails' run, head subrun `fitsColumn` dst]

moveCols :: (Column,Column) -> [(Column,Column)]
moveCols (src,dst) = moveCol (lowestRun src) dst


applyToEveryPair :: ((a,a) -> [(a,a)]) -> [a] -> [[a]]
applyToEveryPair f (xs) = do i<-[0..length xs - 1]
                             j<-[0..length xs - 1]
                             guard (i /= j)
                             (x',y') <- f (xs !! i, xs !! j)
                             return . setList i x' . setList j y' $ xs


moves :: Table -> [Table]
moves = applyToEveryPair moveCols
                             
setList :: Int -> a -> [a] -> [a]
setList = go 0 where
    go _ _ _ [] = []
    go i n x' (x:xs) | i == n    = x':xs
                     | otherwise = x:(go (i+1) n x' xs)

-- this function is just an exercise
applyToEverySingle :: (a->a) -> [a] -> [[a]]
applyToEverySingle f [] = []
applyToEverySingle f (x:xs) = (f x :xs) : (map (x:) (applyToEverySingle f xs))

applyToEverySingle' :: (a->[a]) -> [a] -> [[a]]
applyToEverySingle' f [] = []
applyToEverySingle' f (x:xs) = (map (:xs) (f x) ) ++ (map (x:) (applyToEverySingle' f xs))

atable :: Table
atable = [exampleColumn, [Ace :/ Hearts], [],[R2 :/ Hearts, R3 :/ Clubs], [R3 :/ Hearts, R4 :/ Hearts, R7 :/ Hearts]]
