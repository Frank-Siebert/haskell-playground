-- Fixpoints

fix :: (a -> a) -> a
fix f = let x = f x in x

newtype Fix f = Fix { unFix :: f (Fix f) }

data ListF a b = NilF | ConsF a b

type List a = Fix (ListF a)

toList []     = Fix NilF
toList (x:xs) = Fix (ConsF x (toList xs))

foldList,foldList3 :: (a -> b -> b) -> b -> List a -> b
foldList2 f z (Fix NilF) = z
foldList2 f z (Fix (ConsF x xs)) = f x (foldList f z xs)

foldList f z list = case list of
              (Fix NilF) -> z
              (Fix (ConsF x xs)) -> f x (foldList f z xs)

foldList3 = fix
      (\v f z list -> case list of
              (Fix NilF) -> z
              (Fix (ConsF x xs)) -> f x (v f z xs))

asc :: [Int]
asc = asc' 0 where asc' n = n:asc' (n+1)

ascF = fix (\x y-> y:x (y+1)) 0
ascF' = fix f 0 where
        f x y = y:x (y+1)

to100 = fix (\x y -> if y <= 100 then y:x (y+1) else []) 0
to101 = fix f 0 where
         f     x y | y <= 100  = y:x (y + 1)
                   | otherwise = []

to100' = to100'' 0 where
         to100'' y | y <= 100  = y:to100'' (y+1)
                   | otherwise = []

myFoldr f z [] = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)

myFoldr' f z xs = fix f' z xs where
        f' _ z []     = z
        f' q z (x:xs) = f x (q z xs)

fib n = fix f n where
    f c 0 = 0
    f c 1 = 1
    f c n = c (n - 1) + c (n - 2)
--
-- also, ohne Nachdenken: normale Rekursion aufschreiben,
-- dann als fixpunkt mit zusätzlichem ersten parameter,
-- der an stelle des rekursiven Aufrufs benutzt wird.

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibs2 = let z =    0:1:zipWith (+) z (tail z) in z
fibs3 = let
        z = (\q -> 0:1:zipWith (+) q (tail q)) z in z
fibs4 = fix (\q -> 0:1:zipWith (+) q (tail q))
fibs5 = fix f where
            f q  = 0:1:zipWith (+) q (tail q)

fibs6 = fix f [0,1] where
--   f q (a:b:_) = a:q [b,a+b]
   f q [a,b] = a:q [b,a+b]

fibs7 = fix f [0,1] where
   f q (a:b:whatever) = a:q (b:a+b:whatever)

