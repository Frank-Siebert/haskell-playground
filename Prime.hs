import Data.List(sort)
import System.Environment(getArgs)

primes, primes2 ::  (Integral a) => [a]
primes = [ p | p <-[2..] , prime p]
primes2= 2:[ p | p <-[3,5..] , prime2 p]

--prime :: (Integral a) => a -> Bool
prime, prime2 :: (Integral a) => a -> Bool
prime x = and [not (divisible x y) | y <- [2..sqrtx]]
                where 
				  divisible x y = (x `mod` y) == 0
				  sqrtx = floor . sqrt  . fromIntegral $ x
				  --sqrtx = floor (sqrt (fromIntegral x))

prime2 x = and [not (divisible x y) | y <- takeWhile (\z -> z*z <=x) primes2]
                where 
				  divisible x y = (x `mod` y) == 0
					  

sieve,sieve2old :: (Integral a) => [a]
sieve2old = 2:[n | n <-[3,5..], all (\x -> n `mod` x /= 0) $ takeWhile (\x-> x*x <= n) sieve2old]

sieve = 2:[n | n <-[3,5..], all (\x -> n `mod` x /= 0) $ takeWhile (<= sqrtnat n) sieve]
           where sqrtnat = floor . sqrt  . fromIntegral 
{-
data (Integral a) => Sieve a = Sieve a [a]
sieveUpTo:: (Integral a) => a -> Sieve a
sieveUpTo n | n <=60    = Sieve 60 [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59]
            -- | otherwise = sieveThose (sieveUpTo (n `div` 2))

data (Ord a) => BinTree a = Nil | BinTree (BinTree a) a (BinTree a)
   deriving (Show,Eq)

toTree:: (Ord a) => [a] -> BinTree a
toTree [] = Nil
toTree [x]= BinTree Nil x Nil
toTree ls = let hl = (length ls) `div` 2
                sl = sort ls
                h1 = take hl sl
                c:h2 = drop hl sl
            in BinTree (toTree h1) c (toTree h2)

elemT :: (Ord a) => a -> BinTree a -> Bool
elemT _ Nil = False
elemT x (BinTree left n right) = if n==x then True
                                   else if x < n
                                     then x `elemT` left
                                     else x `elemT` right

-- Mist, ich hänge immer rechts an... == linked list                                     
addT :: (Ord a) => a -> BinTree a -> BinTree a
addT x Nil = BinTree Nil x Nil
addT x t@(BinTree left n right) = if n==x then t
                                   else if x < n
                                     then BinTree (addT x left) n right
                                     else BinTree left n (addT x right)

-}                                 

                           
           
main :: IO ()
main = do args <- getArgs
          putStrLn (show (length (takeWhile (<=numFromArgs args) sieve)))
       where
          numFromArgs:: (Integral a, Read a) => [String] -> a
          numFromArgs [] = 1024*1024
          numFromArgs (x:_) = read x
--main = putStrLn "hello"

primfaktoren :: Integral a => a -> [a]
primfaktoren n = pf n primes2 where
  pf 1 _ =  [];
  pf n aps@(p:ps) = if (n `mod` p == 0) 
                then p:(pf (n `div` p) aps)
				else if p*p <= n then pf n ps else [n]

teiler :: Integral a => a -> [a]
teiler n = filter (\k -> n `mod` k == 0) [1..n]

isPerfect n = n+n == sum (teiler n)

perfects = filter isPerfect [1..]

fibs :: (Num a) => [a]
fibs = 0:1:zipWith (+) fibs (drop 1 fibs)

cart xs ys = -- cartesian product
    xs >>= ( -- klammern überflüssig, die sind nur für mich!
    \x -> (ys >>=
    \y -> return (x, y)))

-- 2010-02-03 
groupBy':: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' cond (x:xs)= groupBy'' [x] x cond xs where
   groupBy'' :: [a] -> a -> (a -> a -> Bool) -> [a] -> [[a]]
   groupBy'' acc last cond (x:xs) = if cond last x
                                       then groupBy'' (x:acc) x cond xs
                                       else acc:groupBy'' [x] x cond xs
   groupBy'' acc _    _     []    = [acc]
   
consec = groupBy' (\x y -> x+1 == y)
unp = filter (not . prime2) [1..]
                                 
