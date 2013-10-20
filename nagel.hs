
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
inspectRight (_,r:_) = r
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
replaceZ (x:xs) (l,_:rs) = replaceZ xs (x:l,rs)
replaceZ xs (l,[]) = replaceZ xs ([], reverse l)


-----------------------------------------------------
type CircList a = ([a]->[a],[a])

circList :: [a] -> CircList a
circList l = (id,l)

advance :: CircList a -> CircList a
advance (f , r:rs) = (f . ((:) r) ,rs)
advance (f , [] ) = advance (id, f [])

isWrap :: CircList a -> CircList a -> Bool
isWrap (_,before) (_,after) = length before > length after

headC :: CircList a -> a
headC (_, r:_) = r
headC (f, []) = head . f $ []

takeC :: Int -> CircList a -> [a]
takeC n c@(_,r:_) = r:takeC (n-1) (advance c)
takeC n (f,[]) = takeC n (id, f [])

advanceWhile :: (a -> Bool) -> CircList a -> CircList a
advanceWhile pred cl = if pred (headC cl) then advanceWhile pred (advance cl) else cl

replaceC :: [a] -> CircList a -> CircList a
replaceC [] cl = cl
replaceC (x:xs) (f, _:rs) = replaceC xs (f . (x:), rs)
replaceC xs (f, []) = replaceC xs (id, f [])

showCm :: (Show a) => CircList a -> String
showCm (f,r) = show (f []) ++ "^" ++ show r

showC :: (Show a) => CircList a -> String
showC (f,r) = show (f r)
------------------------------------------------------

data Cell = Empty | Car Int deriving (Eq)

instance Show Cell where
    show Empty = "."
    show (Car n) = show n

toChar :: Cell -> Char    
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

infiniteRandomList :: [Int]
infiniteRandomList = randoms (mkStdGen 100)

type Road = CircList Cell

-- precondition: The Marker is just before a Car
moveCar :: Road -> Road
moveCar road = let (Car oldSpeed):ahead = takeC (maxSpeed+1) road
                   free = count Empty ahead
                   newSpeed = minimum [oldSpeed + 1, maxSpeed, free]
                in replaceC [Car newSpeed] $ replaceC (replicate newSpeed Empty) road

moveOneCar :: Road -> Road
moveOneCar = moveCar . advanceWhile (==Empty)

makeRoad :: Int -> Road
makeRoad n = (,) id . makeRoad' $ take n infiniteRandomList where
           makeRoad' [] = [] -- todo use map instead!
           makeRoad' (n:ns) = (if (n `mod` (maxSpeed - 1)) == 0 
                                then Car (n `mod` maxSpeed)
                                else Empty):makeRoad' ns

-- debug / test:
rd :: Road
rd = makeRoad 10

mvs' :: [Road]
mvs' = iterate moveOneCar rd

mvs :: Int -> Road -> IO Road
mvs 0 r = return r
mvs n r = putStrLn (showC r) >> mvs (n-1) (moveOneCar r)
-- showC $ mvs' !! 0
