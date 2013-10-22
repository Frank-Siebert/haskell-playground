-- module Second where

import Data.Int (Int32)
import Data.List (tails, inits, sort, group)
import Hashable
import Data.Maybe (fromJust,fromMaybe)
import Control.Monad (liftM)
import System.CPUTime (getCPUTime)
import Debug.Trace (trace,traceShow)

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    putStrLn $ "Computation time: " ++ (show diff)
    return v

ld, ld4, ld5, ld6 :: (Eq a) => [a] -> [a] -> Int
ld [] v = length v
ld u [] = length u
-- ld u@(u1:us) v@(v1:vs) = min (o+(ld us vs)) ((min (ld u vs) (ld us v))+1)
  -- where o = if u1==v1 then 0 else 1

ld u@(u1:us) v@(v1:vs) = if u1==v1
                           then ld us vs
                           else 1+min (ld us vs) (min (ld u vs) (ld us v))

{- -- does not work!
ld2 :: String -> String -> Int
ld2 u v = ld2' u v `get` (u,v) where
          ld2' []        []        = (newHashMap 43 :: HashMap (String,String) Int) `insert` (([],[]) , 0)
          ld2' []        v@(v1:vs) = insert (ld2' [] vs) (([],v),length v)
          ld2' u@(u1:us) []        = insert (ld2' us []) ((u,[]),length u)
          ld2' u@(u1:us) v@(v1:vs) = if u1 == v1
                  then let x = ld2' us vs `insert` ((u,v),fromJust (x `lookUp` (us,vs))) in x 
                  else let d = minimum [fromJust (x `lookUp` (us,vs)),
                                        -- fromJust (x `lookUp` (u ,vs)),
                                        fromJust (x `lookUp` (us,v ))] 
                           x = ld2' us v 
                           z = x `insert` ((u,v),d+1) in z
                           -- x = ld2' us vs `insert` ((u,v),d+1) `insert` ((u,vs),d) `insert` ((us,v),d) `insert` ((u,v),d) in x
                  --else (newHashMap 42 :: HashMap (String,String) Int) `insert` (([],[]) , 0) -- Stub!
-}
ld3 :: (Hashable a) => [a] -> [a] -> Int
--ld3 :: (Hashable a) => [a] -> [a] -> HashMap ([a],[a]) Int
ld3 x y = ld3' x y  `get` (x,y)  where 
          --ld3' :: (Hashable a) => [a] -> [a] -> HashMap ([a],[a]) Int
          ld3' []        []        = newHashMap hashMapSize `insert` (([],[]) , 0)
          ld3' u@(_:us)  []        = ld3' us [] `insert` ((u,[]),length u)
          ld3' []        v@(_:vs)  = ld3' x  vs `insert` (([],v),length v)
          ld3' u@(u1:us) v@(v1:vs) = let eq = if u1/=v1 then 1 else 0
                                         d = minimum [r `get` (us,vs) + eq,
                                                      r `get` (u ,vs) + 1,
                                                      r `get` (us,v ) + 1]
                                         r = ld3' us v in r `insert` ((u,v),d) 
          hashMapSize :: Int32
          hashMapSize = 17 --fromInteger . toInteger $ (length x) * (length y) `div` 2
          -- the entire array is copied (length x) * (length y) times,
          -- without tail-recursion it's not only slow but also out-of-memory -prone
                                     
                                     
          

-- now let's do it with a list comprehension:
--ld4 :: (Eq a) => [a] -> [a] -> Int
ld4'' x y = tail [ (u,v) | u <- reverse . tails $ x, v <-reverse . tails $ y]
ld4' x y = (([],[]),0) : [(p,v p) | p <- ld4'' x y ]
           where v (a,b) | a == [] = length b
                         | b == [] = length a                  
                         | otherwise = minimum [1+fromJust ((     a,tail b) `lookup` recurse),
                                                1+fromJust ((tail a,     b) `lookup` recurse),
                                                d+fromJust ((tail a,tail b) `lookup` recurse)]
                                        where d = if head a == head b then 0 else 1
                                              recurse = ld4' x y
                                                
                     
ld4 x y = fromJust ((x,y) `lookup` ld4' x y)

ld5'' x y = tail [ (u,v) | u <- [0..length x], v <- [0..length y]]
ld5' x y = ((0,0),0) : [(p,v p) | p <- ld5'' x y ]
           where v (a,b) | a == 0 = b
                         | b == 0 = a                  
                         | otherwise = minimum [1+fromJust ((a  ,b-1) `lookup` ld5' x y),
                                                1+fromJust ((a-1,b  ) `lookup` ld5' x y),
                                                d+fromJust ((a-1,b-1) `lookup` ld5' x y)]
                                        where d = if x !! (a-1)== y !! (b-1) then 0 else 1

ld5 x y = snd . last $ (ld5' x y)

ld6'' x y = tail [ (u,v) | u <- [0..length x], v <- [0..length y]]
ld6' x y = ((0,0),0) : [(p,v p) | p <- ld6'' x y ]
           where v (a,b) | a == 0 = b
                         | b == 0 = a                  
                         | otherwise = minimum [1+fromJust ((a  ,b-1) `lookup` recurse),
                                                1+fromJust ((a-1,b  ) `lookup` recurse),
                                                d+fromJust ((a-1,b-1) `lookup` recurse)]
                                        where d = if x !! (a-1)== y !! (b-1) then 0 else 1
                                              recurse = ld6' x y

ld6 x y = snd . last $ (ld6' x y)


main :: IO ()
main = do
         putStrLn "Enter two strings, separated by [return]"
         x <- getLine
         y <- getLine                             -- ld2 broken!
         sequence_ [ time $ putStrLn ( show (ald x y)) | ald <-[ld3,ld,ld4,ld5,ld6]]
                                        
output = zipWith (,) (ld4' "blah" "blub") (ld6' "blah" "blub")

wavefront :: b -> ([a] -> b -> b) -> ([a] -> b -> b) -> ([a] -> [a] -> b -> b -> b -> b)-> [a] -> [a] -> b
wavefront ul _  _  _ []       []       = ul
wavefront ul dn ri f u@(_:us) []       = (dn u (wavefront ul dn ri f us []))
wavefront ul dn ri f []       v@(_:vs) = (ri v (wavefront ul dn ri f [] vs))
wavefront ul dn ri f u@(_:us) v@(_:vs) = f u v (wavefront ul dn ri f us v )
                                               (wavefront ul dn ri f u  vs)
                                               (wavefront ul dn ri f us vs)

ldw :: (Eq a) => [a] -> [a] -> Int
ldw = wavefront 0 (\x _ -> length x) (\x _ -> length x) 
                  (\x y up le ul -> if head x == head y then ul
                                      else 1 + minimum [up,le,ul])
                                      
shuffle :: (Ord a) => [a] -> [a] -> [[a]]
shuffle = wavefront [[]] (\w _ -> [w]) (\w _ -> [w])
 (\(u:_) (v:_) up le _ -> 
   liftM (u:) up   +-+
   map   (v:) le)
   
(+-+) :: (Ord a) => [a] -> [a] -> [a]
x +-+ y = map head $ group . sort $ x++y
-- x +-+ y = map head (group  (sort (x++y)))

{-
wf3 :: b -> ( b-> Maybe (a,b) -> Maybe (a,b) -> b) -> [a] -> [a] -> b
wf3 init f [] [] = init
wf3 init f [] v:vs = f init (Just (v,(wf init f [] vs))) Nothing 
-}
charac :: (Eq a) => a -> a -> Int
charac x y = if x == y then 0 else 1

type LevenResult a = (Int, Alignment a)
--type LevenResult a = Int

alignToEmpties :: [a] -> [LevenResult a]
alignToEmpties x = map (\s -> (length s,reverse $ map (\z -> (Nothing, Just z)) s)) . inits $ x

alignToEmpty2 x = init . map (\s -> (length s,reverse $ map (\z -> (Just z, Nothing)) s)) . inits $ x

ld0:: (Eq a, Ord a ,Show a) => [a] -> [a] -> LevenResult a
ld0 x y = (\(z,s) -> (z, reverse s)) . head . reverse $ reihe (alignToEmpties y) (alignToEmpty2 x) x y

traceShow' arg = traceShow arg arg

zeile :: (Eq a, Ord a,Show a) => [LevenResult a] -> LevenResult a -> a -> [a] -> [LevenResult a]
zeile [u] left _ [] = [left]
zeile (leftUp:up:ups) left c (y:ys) = let new = minimum_alignment leftUp left up c y
                                       in left:zeile (up:ups) new c ys


reihe :: (Eq a,Ord a,Show a) => [LevenResult a] -> [LevenResult a] -> [a] -> [a] -> [LevenResult a]
reihe ups _ [] _ = ups
reihe ups (z:zs) (x:xs) y = reihe (zeile ups z x y) zs xs y

data Edit a = Same a | Replace a a | Insert a | Delete a
-- what is the best way to display the changes? 3 line with center line one of '=', '/', 'v', '^'?

type Alignment a = [(Maybe a,Maybe a)]
toString :: Alignment Char -> String
toString a = "\n" ++ map (fromMaybe ' ' . fst) a ++ "\n" ++ map (fromMaybe ' ' . snd) a

align :: (Eq a,Ord a) => [a] -> [a] -> (Int, Alignment a)
align [] [] = (0, [])
align [] y  = (length y, map (\z -> (Nothing, Just z)) y)
align x  [] = (length x, map (\z -> (Just z, Nothing)) x)
align x@(x1:xs) y@(y1:ys) = minimum_alignment (align xs ys) (align x ys) (align xs y) x1 y1

minimum_alignment :: (Eq a,Ord a) => (Int, Alignment a) -> (Int, Alignment a) -> (Int, Alignment a) -> a -> a -> (Int,Alignment a)
minimum_alignment ul le up x1 y1 = let narf (v, ament) a@(x,y) = (v + charac x y, a:ament)
                                    in minimum [narf ul (Just x1, Just y1),
                                                narf le (Nothing, Just y1),
                                                narf up (Just x1, Nothing)]

al :: String -> String -> IO ()
al x y = let (value, a) = ld0 x y
          in putStrLn $ show value ++ toString a
