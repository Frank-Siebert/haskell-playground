-- play around which defintions of unwrap and duplicate make lists a comonad
import Data.List (tails,inits)

-- does not work in ghci, but can be compiled by ghc with -XRankNTypes

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RankNTypes #-}

--test :: (Functor f, Eq (f a), Show (f a),Eq (f (f a)),Show (f (f a))) => 
--        (f a -> a) -> (f b -> f (f b)) -> f a -> IO ()
test :: (forall a. [a] -> a) -> (forall b. [b] -> [[b]]) -> [Int] -> IO ()
test unwrap duplicate dat = do
  let dupdat = duplicate dat
  checkEq "fmap id            == id" dat (fmap id dat)
  checkEq "unwrap . duplicate == id" dat (unwrap dupdat)
  checkEq "fmap unwrap . dupl == id" dat (fmap unwrap dupdat)
  checkEq "dup . dup==fmap dup .dup" (duplicate dupdat) (fmap duplicate dupdat)
  
checkEq :: (Eq a, Show a) => String -> a -> a -> IO ()
checkEq msg a b = do
  putStrLn $ msg ++ if a == b
    then " ok"
	else "! "++show a ++" /= "++show b
  

--main :: IO ()
main = do test head (init . tails) [1..4]
          test last (reverse . init . tails) [1..4] -- fails
          test last (tail . inits) [1..4]
          test head (\ls -> replicate (length ls) ls) [1..4] -- seems ok, but NOK with fmap unwrap . duplicate = id

unun :: (Functor f) => f a -> a
unun = undefined

undp :: (Functor f) => f a -> f (f a)
undp = undefined
