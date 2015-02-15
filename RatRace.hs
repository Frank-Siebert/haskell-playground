{-# Language DeriveFunctor #-}

import Control.Applicative
import Control.Monad.State
import Control.Comonad
import Data.Maybe (catMaybes)
import System.Random

raceTrackLength :: Int
raceTrackLength = 53

raceTrackWidth :: Int
raceTrackWidth = 15

genomeLength :: Int
genomeLength = 100

genomeChangeChance :: Double
genomeChangeChance = 0.05

genomeFlipChance :: Double
genomeFlipChance = 0.01


type Genome = [Bool]
type Color = Int

type Vision = U2 Color

data Move = StandStill | North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest


type Player = Genome -> (StdGen -> Vision -> Move)

type Rand a = State StdGen a

randomGenome :: Rand Genome
randomGenome = replicateM 100 getRandom

getRandom :: (Random a) => Rand a
getRandom = do (x,g) <- (random <$> get)
               put g
               return x

getRandomR :: (Random a) => (a,a) -> Rand a
getRandomR range = do (x,g) <- (randomR range <$> get)
                      put g
                      return x

getStdGen :: Rand StdGen
getStdGen = do (g,h) <- (split <$> get)
               put g
               return h

mixGenome :: Genome -> Genome -> Rand Genome
mixGenome mother father = do coin <- getRandom
                             mix (if coin
                                   then (mother,father)
                                   else (father,mother))
                          where
                             mix ([],recessive) = return recessive
                             mix (d:ominant,recessive) = (d:) <$>
                                     do x <- getRandom
                                        mix (if (x < genomeChangeChance)
                                               then (recessive,ominant)
                                               else (ominant,recessive))

mutateGenome :: Genome -> Rand Genome
mutateGenome [] = return []
mutateGenome (g:gs) = do c <- getRandom
                         ((if c < genomeFlipChance then not g else g):) <$>  mutateGenome gs


main :: IO ()
main = do newStdGen >>= print  . evalState randomGenome
          putStrLn "Now for some more stuff"
          newStdGen >>= print . evalState generateRaceTrack
          newStdGen >>= print . evalState generateCells

data U a = U [a] a [a] deriving (Show,Eq,Functor)

rightU :: U a -> Maybe (U a)
rightU (U _ _ []) = Nothing
rightU (U ls x (r:rs)) = Just (U (x:ls) r rs)

leftU :: U a -> Maybe (U a)
leftU (U [] _ _) = Nothing
leftU (U (l:ls) x rs) = Just (U ls l (x:rs))

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f z = case f z of
                Nothing -> []
                Just z' -> z':iterateMaybe f z'

rightUs :: U a -> [U a]
rightUs = iterateMaybe rightU

leftUs :: U a -> [U a]
leftUs =iterateMaybe leftU

instance Comonad U where
    extract (U _ x _) = x
    duplicate z = U (leftUs z) z (rightUs z)

newtype U2 a = U2 (U (U a)) deriving (Show,Eq,Functor)

instance Comonad U2 where
    extract (U2 u) = extract . extract $ u
    duplicate (U2 u) = fmap U2 . U2 . duplicate . duplicate $ u

rightU2 :: U2 a -> Maybe (U2 a)
rightU2 (U2 u) = fmap U2 . rightU $ u

listU :: ([a] -> [a]) -> U a -> U a
listU f (U ls x rs) = U (f ls) x (f rs)

-- a real listU2 function would need higher rank:
--(forall a.[a] -> [a]) -> U2 a -> U2 a
takeU2 :: Int -> U2 a -> U2 a
takeU2 n (U2 (U ls x rs)) = U2 . fmap (listU (take n)) $ U (take n ls) x (take n rs)

fromListU :: [a] -> U a
fromListU (x:xs) = U [] x xs
fromListU _ = error "fromListU: Empty list"

fromListU2 :: [[a]] -> U2 a
fromListU2 (x:xs) = U2 (U [] (fromListU x) (map fromListU xs))
fromListU2 _ = error "fromListU2: Empty list"

----- below here controller only

type Position = (Int,Int)

-- technically, Empty == Teleporter 0 0
data Cell = Wall | Teleporter Int Int | Trap Int Int deriving (Eq,Show)
emptyCell :: Cell
emptyCell = Teleporter 0 0

data Specimen = Specimen { genome :: Genome, completedRuns :: Int } --

generateRaceTrack :: Rand (U2 (Position,Color))
generateRaceTrack = fromListU2 . addPos <$> replicateM raceTrackWidth (replicateM raceTrackLength generateColor)

addPos :: [[a]] -> [[(Position,a)]]
addPos = zipWith (\y -> map (\(x,a)->((x,y),a))) [0..] . map (zip [0..])

generateColor :: Rand Color
generateColor = do (x,g) <- randomR (0,15) <$> get
                   put g
                   return x

generateCells :: Rand [Cell]
generateCells = do te1 <- genCell Teleporter 4
                   te2 <- genCell Teleporter 4
                   tr1 <- genCell Trap 4
                   tr2 <- genCell Trap 4
                   shuffle $ [Wall,Wall,te1,te2,tr1,tr2]++replicate 8 emptyCell where
           genCell ctor r = dropWhileM (==ctor 0 0) $ liftM2 ctor (getRandomR (-r,r)) (getRandomR (-r,r))

dropWhileM :: (Monad m) => (a -> Bool) -> m a -> m a
dropWhileM p action = do x <- action
                         if p x then dropWhileM p action else return x

shuffle :: [a] -> Rand [a]
shuffle xs = go (length xs) xs where
             go 1 a = return a
             go n as = do i <- getRandomR (0,n - 1)
                          let remainder = take i as ++ drop (i+1) as
                          res <- go (n-1) remainder
                          return $ (as!!i):res

{- orphan, I do not really need you
instance (Random a, Random b) => Random (a,b) where
    random g = let (a,g' ) = random g
                   (b,g'') = random g' in ((a,b),g'')
    randomR ((la,lb),(ha,hb)) g = let (a,g' ) = randomR (la,ha) g
                                      (b,g'') = randomR (lb,hb) g' in ((a,b),g'')
-}

data FullCell = FullCell {
   vision   :: U2 Color,
   nextCell :: Maybe FullCell, -- Nothing means specimen dies
   position :: Position,
   cellType :: Cell,
   move     :: Move -> Maybe (U2Graph FullCell)
}

iter :: (Monad m) => Int -> (a -> m a) -> a -> m a
iter 0 _ = return
iter n f = f >=> iter (n-1) f

sgnToMove :: Int -> U a -> Maybe (U a)
sgnToMove 0 = Just
sgnToMove x | x > 0 = iter   x rightU
sgnToMove x | x < 0 = iter (-x) leftU

upMaybeU :: U (Maybe a) -> Maybe (U a)
upMaybeU (U _ Nothing _) = Nothing
upMaybeU (U ls (Just x) rs) = Just (U (catMaybes ls) x (catMaybes rs))

moveFocus :: Position -> U2 a -> Maybe (U2 a)
--moveFocus (x,y) (U2 u) = U2 <$> sgnToMove y u
moveFocus (x,y) (U2 u) = U2 <$> ( upMaybeU . (fmap (sgnToMove x)) =<< sgnToMove y u)

buildFullCell :: [Cell] -> U2 (Position,Color) -> U2Graph FullCell
buildFullCell cards track = toU2GraphW b track where
    b this u = FullCell {
        vision   = snd <$> (takeU2 2 u),
        nextCell = Nothing, -- TODO
        position = fst $ extract u,
        cellType = cards !! (snd $ extract u),
        move     = undefined
    }

data U2Graph a = U2Graph {
    _down2  :: Maybe (U2Graph a),
    _left2  :: Maybe (U2Graph a),
    _here2  :: a,
    _right2 :: Maybe (U2Graph a),
    _up2    :: Maybe (U2Graph a)
}

square :: U2 Char
square = fromListU2 ["ab","cd"]


-- from http://stackoverflow.com/questions/28516819/tying-the-knot-with-a-comonad
toU2Graph :: (U2Graph b -> a -> b) -> U2 a -> U2Graph b
toU2Graph c (U2 (U ls (U ds h us) rs)) = g
    where
        g = U2Graph (build u2down g ds) (build u2left g ls) (c g h) (build u2right g rs) (build u2up g us)
        build _ _    []            = Nothing
        build f prev (here:theres) = Just g'
            where
                g' = f (Just prev) here (build f g' theres)
        u2up   d h u = let g' = U2Graph d (d >>= _left2 >>= _up2  ) (c g' h) (d >>= _right2 >>= _up2  ) u in g'
        u2down u h d = let g' = U2Graph d (u >>= _left2 >>= _down2) (c g' h) (u >>= _right2 >>= _down2) u in g'
        u2left r (U ds h us) l = g'
            where
                g' = U2Graph (build u2down g' ds) l (c g' h) r (build u2up g' us)
        u2right l (U ds h us) r = g'
            where
                g' = U2Graph (build u2down g' ds) l (c g' h) r (build u2up g' us)

toU2GraphW :: (U2Graph b -> U2 a -> b) -> U2 a -> U2Graph b
toU2GraphW f u = toU2Graph f (duplicate u)
