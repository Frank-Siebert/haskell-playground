
import System.Random (randoms, mkStdGen)
import Data.CircList
import Data.List (intercalate)
import Control.Comonad

data Cell = Empty | Car Int deriving (Eq)

instance Show Cell where
    show Empty = "."
    show (Car n) = show n

toChar :: Cell -> Char    
toChar Empty = '.'
toChar (Car n) = Prelude.head . show $ n

count :: (Eq a) => a -> [a] -> Int
count x = length . takeWhile (x==)

maxSpeed :: Int
maxSpeed = 5

infiniteRandomList :: [Int]
infiniteRandomList = randoms (mkStdGen 100)

type Road = CircList Cell

cl :: [a] -> CircList a
cl (x:xs) = mkCircList x xs

makeRoad :: Int -> Road
makeRoad len = cl (makeRoad' $ take len infiniteRandomList) where
           makeRoad' = map (\n -> if (n `mod` (maxSpeed - 1)) == 0 
                                then Car (n `mod` maxSpeed)
                                else Empty)

-- debug / test:
rd :: Road
rd = makeRoad 10

updateSpeed :: [Cell] -> Cell
updateSpeed (Empty:_) = Empty
updateSpeed (Car curSpeed:ahead) = let freeRoad = count Empty ahead
                                       newSpeed = minimum [curSpeed + 1, freeRoad, maxSpeed]
                                    in Car newSpeed

-- We need to do something to move all cars according to their speed...

move :: [Cell] -> Cell
move = move' 0 where
       move' n (c@(Car m):_) | n==m = c
       move' n (_:xs)     | n>maxSpeed = Empty
                          | otherwise = move' (n+1) xs

nagel :: Road -> Road
nagel r = let a = r =>> toInfList
              b = fmap updateSpeed a
              c = b =>> move . toInfList . reverseC
           in c

display :: [Road] -> String
display = intercalate "\n" . map (map toChar . toFiniteList)

main :: IO ()
main = putStr . display . take 20 . iterate nagel . makeRoad $ 100

