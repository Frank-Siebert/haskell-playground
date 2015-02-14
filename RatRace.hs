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
   vision :: U2 Color,
   next   :: Maybe FullCell, -- Nothing means specimen dies
   position :: Position,
   cellType :: Cell,
   move     :: Move -> Maybe FullCell
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

--buildFullCell :: [Cell] -> U2 (Position,Color) -> U2 FullCell
buildFullCell cards track = track =>> (\x -> let color = snd $ extract x
                                                 cell  = cards !! color
                                                 tmp   = takeU2 2 x
                                                 vis   = snd <$> tmp
                                                 pos   = fst $ extract x
                                              in (vis, pos, cell))

data Tied b a = Tied a (b (Tied b a))
