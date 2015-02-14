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
   vision   :: Int,
   move     :: Int -> Maybe FullCell -- tie the knot here!
}


tieKnot :: U Int -> U FullCell
tieKnot u = u =>> (\z -> FullCell {
      vision = limitTo5x5 z,
      move = move'
})  where
         move'   1  = Just undefined -- tie the knot to neighbor here
         move' (-1) = Just undefined -- ...
         move'   _  = Nothing
         limitTo5x5 = extract -- simplified for SO

type MT a = Maybe (T a) -- dont export
data T a = T (Maybe (T a)) a (Maybe (T a))

tie :: U a -> T a
tie   (U ls x rs) = let center = T (tieLeft ls jcenter) x (tieRight rs jcenter)
                        jcenter = Just center
                        tieLeft  :: [a] -> Maybe (T a) -> Maybe (T a)
                        tieLeft [] right = Nothing
                        tieLeft (l:ls) right = let this = Just $ T (tieLeft ls this) l right in this
                        tieRight :: [a] -> Maybe (T a) -> Maybe (T a)
                        tieRight [] left = Nothing
                        tieRight (r:rs) left = let this = Just $ T left r (tieRight rs this) in this
                     in center
