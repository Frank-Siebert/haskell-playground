{-# LANGUAGE ScopedTypeVariables #-}
import System.Environment ( getArgs )

allFunctions :: (Eq a, Enum a, Bounded a, Enum b, Bounded b) => [a -> b]
allFunctions = f minBound
    where
       f a | a == maxBound = [\x -> b|b <- [minBound .. maxBound]]
           | otherwise = [(\x -> if x==a then b else f' x) |
                                                 f'<-f (succ a),
                                                 b<-[minBound .. maxBound]]

allFunctions' :: (Eq a) => [a] -> [b] -> [a -> b]
allFunctions' [] _ = [undefined{- sic! -}] -- a->b, b^a, b^0 = 1
allFunctions' [_] bs = fmap const bs
allFunctions' (a:as) bs = [ \x -> if x==a then b else f x |
                                 f <-allFunctions' as bs,
                                 b <-bs]

ff :: [Bool->Bool]
ff = allFunctions

newtype E2 a b = E2 {getE2::(a,b)} deriving (Eq,Show,Read,Bounded)
instance (Enum a,Enum b,Bounded b) => Enum (E2 a b) where
    succ (E2 (a,b)) | [_] <- [b..maxBound] = E2 (succ a, minBound)
                    | otherwise = E2 (a, succ b)
    pred (E2 (a,b)) | [_] <- [minBound..b] = E2 (pred a, maxBound)
                    | otherwise = E2 (a, pred b)
    toEnum n = let range = fromEnum (maxBound::b) - fromEnum (minBound::b) + 1
                   (d,m) = divMod n range
                in E2 (toEnum d,toEnum m)
    fromEnum (E2 (a,b)) = fromEnum b + (fromEnum (maxBound::b) - fromEnum (minBound::b)+1)*fromEnum a
    {- slightly faster if commented out!
    enumFromTo x@(E2 (a,b)) (E2 (a',b'))
       | [] <- [a..a'] = []
       | [_] <- [a..a'] = [E2 (a,b'') | b'' <-[b,b']]
       | (a'':a''':_) <- [a',pred a' .. a] = enumFromTo x (E2 (a''',maxBound))
                                           ++ [E2 (a'',b'') | b''<-[minBound..b']]
    -}

data Four = One|Two|Three|Four deriving (Eq,Bounded,Enum,Show,Read)

main = do (a:b:_) <- fmap read <$> getArgs
          print (length (allFunctions' [1..a] [1..b]))
