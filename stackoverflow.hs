{-# Language DeriveFunctor #-}

import Control.Comonad

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


data FullCell = FullCell {
   vision   :: [[Int]],
   move     :: Int -> Maybe (U FullCell) -- tie the knot here!
}


tieKnot :: U Int -> U FullCell
tieKnot u = let result = u =>> (\z -> FullCell {
                                   vision = limitTo5x5 z,
                                   move = move'
                              })
                move'   1  = Just undefined -- tie the knot to neighbor here
                move' (-1) = rightU result -- WRONG: Now result is centered initial focus
                move'   _  = Nothing
                limitTo5x5 = undefined -- not of interest, but the real reason why a comonad is used
    in result

type MT a = Maybe (T a) -- dont export
--data T a = T (Maybe (T a)) a (Maybe (T a))
data T a = T { leftT :: MT a, payloadT :: a, rightT :: MT a }

tie :: U a -> T a
tie   (U ul x ur) = let center = T (tieLeft ul jcenter) x (tieRight ur jcenter)
                        jcenter = Just center
                        tieLeft  :: [a] -> Maybe (T a) -> Maybe (T a)
                        tieLeft []     _     = Nothing
                        tieLeft (l:ls) right = let this = Just $ T (tieLeft ls this) l right in this
                        tieRight :: [a] -> Maybe (T a) -> Maybe (T a)
                        tieRight []     _    = Nothing
                        tieRight (r:rs) left = let this = Just $ T left r (tieRight rs this) in this
                     in center

tie' :: [a] -> T a
tie' xs = let tmp = [T (safeIndex (i-1)) (xs !! i) (safeIndex (i+1))| i <-[0..n]]
              n   = length xs - 1
              safeIndex i | i < 0 = Nothing
                          | i > n = Nothing
                          | otherwise = Just (tmp !! i)
           in head tmp

unTie :: T a -> U a
unTie x = U (iterateT leftT x) (payloadT x) (iterateT rightT x) where
    iterateT f t = case f t of
        Nothing -> []
        Just t' -> payloadT t' : iterateT f t'


com :: Int -> [a] -> [[a]]
com 0 ls = [[]]
--com 1 ls = [[x] | x <- ls]
com n ls = [ x : shorter | x <- ls, shorter <- com (n - 1) ls ]