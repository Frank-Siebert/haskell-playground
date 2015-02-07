{-# Language DeriveFunctor #-}

import System.Random
import Control.Monad.State
import Control.Applicative
import Control.Comonad

genomeLength :: Int
genomeLength = 100

genomeChangeChance :: Double
genomeChangeChance = 0.05

genomeFlipChance :: Double
genomeFlipChance = 0.01


type Genome = [Bool]
type Color = Int

type Vision = [[Color]]

data Move = StandStill | North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest


type Player = Genome -> (StdGen -> Vision -> Move)

type Rand a = State StdGen a

randomGenome :: Rand Genome
randomGenome = replicateM 100 getRandom

getRandom :: (Random a) => Rand a
getRandom = do (x,g) <- (random <$> get)
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
                             mix ([],[]) = return []
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
main = newStdGen >>= print  . evalState randomGenome

data U a = U [a] a [a] deriving (Show,Eq,Functor)
data U2 a = U2 (U (U a))

rightU :: U a -> Maybe (U a)
rightU (U _ _ []) = Nothing
rightU (U ls x (r:rs)) = Just (U (x:ls) r rs)

leftU :: U a -> Maybe (U a)
leftU (U [] _ _) = Nothing
leftU (U (l:ls) x rs) = Just (U ls l (x:rs))

iterateU :: (U a -> Maybe (U a)) -> U a -> [U a]
iterateU f z = case f z of
                Nothing -> []
                Just z' -> z':iterateU f z'

rightUs :: U a -> [U a]
rightUs = iterateU rightU

leftUs :: U a -> [U a]
leftUs =iterateU leftU

instance Comonad U where
    extract (U _ x _) = x
    duplicate z@(U ls _ rs) = (U undefined z undefined) 

