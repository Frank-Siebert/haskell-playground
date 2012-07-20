-- module Second where

import Data.Int (Int32)
import Data.List (tails,sort,group)
import Hashable
import Maybe (fromJust)
import Monad (liftM)

ld :: (Eq a) => [a] -> [a] -> Int
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
         x <- getLine
         y <- getLine                             -- ld2 broken!
         seq_ [ putStrLn ( show (ald x y)) | ald <-[ld3,ld,ld4,ld5,ld6]]
         return ()
         
seq_ :: [IO ()] -> IO ()
seq_ = foldr (>>) (return ())
                                        
output = zipWith (,) (ld4' "blah" "blub") (ld6' "blah" "blub")

wavefront :: b -> ([a] -> b -> b) -> ([a] -> b -> b) -> ([a] -> [a] -> b -> b -> b -> b)-> [a] -> [a] -> b
wavefront ul _  _  f []       []       = ul
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