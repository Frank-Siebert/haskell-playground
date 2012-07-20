import Control.Monad
import Data.List(sort)

main :: IO ()
main = do t <- getLine
          sequence_ [getTestCase i | i <- [1..read t]]
          
getTestCase :: Int -> IO ()
getTestCase i = do
          n <- getLine
          av <- getVector
          bv <- getVector
          when ( length av == read n && length bv == read n) (do
            putStr ("Case #"++ show i ++ ": ")
            print $ minimizeVectors av bv)

minimizeVectors :: [Integer] -> [Integer] -> Integer
minimizeVectors a b = let a' = sort a
                          b' = reverse . sort $ b
                      in sum $ zipWith (*) a' b'    
            
getVector :: IO [Integer]
getVector = do
                 line <- getLine
                 let ws = words line in
                    return (map read ws)
                 
data S5 = S5 Integer Integer deriving (Eq,Show)

instance Num S5 where
  (S5 a b) + (S5 a' b') = S5 (a+a') (b+b')
  (S5 a b) - (S5 a' b') = S5 (a-a') (b-b')
  (S5 a b) * (S5 a' b') = S5 (a*a'+5*b*b') (a*b'+a'*b)
  abs (S5 a b) = S5 0 0 
  signum (S5 a b) = 0
  fromInteger a = (S5 a 0)
  

                 