
import System.Random (random, randoms, mkStdGen, StdGen, next)
import Control.Monad (replicateM)
--import Control.Monad.Random
--import Control.Monad.Random.Class

-- the marker / head is always between 2 elements!
type Zipper a = ([a],[a])


atEnd :: Zipper a -> Bool
atEnd (_,[]) = True
atEnd ([],_) = True
atEnd _ = False

inspectRight :: Zipper a -> a
inspectRight (_,r:rs) = r
inspectRight (l,[]) = last l

goRight :: Zipper a -> Zipper a
goRight (l, r:rs) = (r:l,rs)
goRight (l, []) = ([], reverse l)

goLeft :: Zipper a -> Zipper a
goLeft (l:ls,r) = (ls, l:r)
goLeft ([],r) = (reverse r,[])

takeZ :: Int -> Zipper a -> [a]
takeZ 0 _ = []
takeZ n (l,r) = take n . cycle $ (r ++ reverse l)

-- Replaces the next elements by the list, advances the zipper.
replaceZ :: [a] -> Zipper a -> Zipper a
replaceZ [] z = z
replaceZ (x:xs) (l,r:rs) = replaceZ xs (x:l,rs)
replaceZ xs (l,[]) = replaceZ xs ([], reverse l)

data Cell = Empty | Car Int deriving (Eq)

instance Show Cell where
    show Empty = "."
    show (Car n) = show n
    
toChar Empty = '.'
toChar (Car n) = head . show $ n

{-
instance Show ([] Cell) where
    show []     = ""
    show (x:xs) = show x : show xs
-}

count :: (Eq a) => a -> [a] -> Int
count _   []  = 0
count a (b:bs)= if a==b then 1 + count a bs else 0

maxSpeed :: Int
maxSpeed = 5

process :: [Cell] -> [Cell]
process ls = encycle (length ls) $ process' (ls ++ take maxSpeed (cycle ls)) where
  -- process' :: [Cell] -> [Cell] -> [Cell] -- on parameter infinite, the other one finite? TODO
  process' (Empty:ls)     = Empty : process' ls
  process' (Car speed:ls) = let freeRoad = count Empty ls
                                newSpeed = minimum [speed+1,freeRoad,maxSpeed]
                             in replicate newSpeed Empty ++ (Car newSpeed:process' (drop newSpeed ls))
  process' [] = []
  encycle n ls = adds (take n ls) (take n $ drop n ls)
                             
add :: Cell -> Cell -> Cell
add Empty c = c
add c Empty = c

adds :: [Cell] -> [Cell] -> [Cell]
adds [] [] = []
adds l [] = l
adds [] l = l
adds (a:as) (b:bs) = (add a b):adds as bs
                                

--mpf :: StdGen
--mpf = getRandom (random (mkStdGen 100)) 

--randRoad :: Int -> [Cell]
--randRoad n = mpf >> replicateM n (return . next)
                
infiniteRandomList :: [Int]
infiniteRandomList = randoms (mkStdGen 100)


