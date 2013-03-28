-- Burrows-Wheeler.

import Data.List (sort,elemIndex,nub)
import Data.Maybe (fromJust)

transform' :: (Ord x) => [x] -> ([x],Int)
transform' x = let n = length x
                   xx = x++x
                   matrix = sort $ [take n $ drop i $ xx | i<-[0.. (n-1)]]
               in
                  (map last matrix,fromJust $ elemIndex x matrix)
                 
transform :: (Ord x) => [x] -> ([x],Int)
transform x = let n = length x
                  line i = take n $ drop i $ x++x
                  matrix = sort [line i | i<-[0..(n-1)]]
              in
                 (map last matrix,fromJust $ elemIndex x matrix)
                 
                 
reTransform :: (Ord x) => ([x],Int) -> [x]
reTransform (x,n) = let tp = zip x [0..]
                        len = length x
                        sx = sort tp
                        p :: [Int] -- permutation, index i is moved to p !! i
                        p = map snd sx
                        --f z = map ( z !! ) p
                        f = flip map p . (!!) -- another point-free exercise solved.
                    in
                        map (x !!) [ l !! n | l<-take len $ iterate f p]
                        --map (x !!) [ l !! n | l<-iterate f p] -- wrong!

hamster = "Es war einmal ein Hamster, der hatte der Weiber vier. "
pg = "Polygamie, Polygamo, Polygamiahiao. "
timmy = hamster++hamster++pg++pg

collect :: (Ord a) => [a] -> [a]
collect = nub . sort

--mtf x = mtf' x (collect x) [0..
-- mtf nutzt ganz stark aus, dass char = byte...
