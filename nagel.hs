
import System.Random (random, randoms, mkStdGen, StdGen, next)
import Control.Monad (replicateM)
--import Control.Monad.Random
--import Control.Monad.Random.Class


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
process ls = take (length ls) $ process' ls ++ take maxSpeed (cycle ls) where
  process' (Empty:ls)     = Empty : process' ls
  process' (Car speed:ls) = let freeRoad = count Empty ls
                                newSpeed = minimum [speed+1,freeRoad,maxSpeed]
                             in replicate newSpeed Empty ++ (Car newSpeed:drop newSpeed ls)
                                

--mpf :: StdGen
--mpf = getRandom (random (mkStdGen 100)) 

--randRoad :: Int -> [Cell]
--randRoad n = mpf >> replicateM n (return . next)
                
infiniteRandomList :: [Int]
infiniteRandomList = randoms (mkStdGen 100)


