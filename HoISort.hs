
import List (sortBy)
import Data.Char (toUpper)


main :: IO ()
main = do
          lns <- getContents
          putStrLn . unlines $ (sortBy smartCmp (lines lns))
            
            
smartCmp :: String -> String -> Ordering
smartCmp u v = compare (recode u) (recode v)

recode :: String -> String
recode w = recode' (Just 0) (map toUpper w) where
           recode' :: Maybe Int -> String -> String
           recode' Nothing [] = []
           recode' Nothing (' ':w) = recode' (Just 0) w
           recode' Nothing (c:w) = c : (recode' Nothing w)
           recode' (Just n) [] = fillLeft '0' 8 (show n) -- add a bunch of leading zeros!
           recode' (Just n) ('0':w) = recode' (Just (n*10  )) w
           recode' (Just n) ('1':w) = recode' (Just (n*10+1)) w
           recode' (Just n) ('2':w) = recode' (Just (n*10+2)) w
           recode' (Just n) ('3':w) = recode' (Just (n*10+3)) w
           recode' (Just n) ('4':w) = recode' (Just (n*10+4)) w
           recode' (Just n) ('5':w) = recode' (Just (n*10+5)) w
           recode' (Just n) ('6':w) = recode' (Just (n*10+6)) w
           recode' (Just n) ('7':w) = recode' (Just (n*10+7)) w
           recode' (Just n) ('8':w) = recode' (Just (n*10+8)) w
           recode' (Just n) ('9':w) = recode' (Just (n*10+9)) w
           recode' (Just n) ('C':w) = recode' (Just (n+ 100)) w
           recode' (Just n) ('L':w) = recode' (Just (n+  50)) w
           recode' (Just n) ('X':w) = recode' (Just (roman n 10)) w
           recode' (Just n) (c:w) = (fillLeft '0' 8 (show n))++[c]++ (recode' Nothing w) -- leading zeros!
           roman acc digit | acc 
           
fillLeft :: a -> Int -> [a] -> [a]
fillLeft p n w = addLeft p (n - length w) w where
                 addLeft p n w | n <= 0    = w
                               | otherwise = p : (addLeft p (n-1) w)
            
