import System.IO
import Control.Monad
import Data.Function(on)
import Data.List(genericLength,groupBy,sort,(\\))
import Data.Maybe(catMaybes,fromJust)

data Beep = Short | Long deriving (Eq,Ord,Show)
type Morse = [Beep]

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.

    input_line <- getLine
    let morse = input_line :: String
    input_line <- getLine
    let n = read input_line :: Int

    dictLines <- replicateM n getLine
    -- putStrLn $ morse++show n ++ show dictLines
    --print $ mkTree (map encode ["E","E"])
    let answer = processRaw morse dictLines
    --let answer = processRaw ".." ["E"]
    -- hPutStrLn stderr "Debug messages..."

    -- Write answer to stdout
    print answer

encode :: String -> Morse
encode = concatMap encodeChar

encodeChar :: Char -> Morse
encodeChar 'A' = [Short,Long]
encodeChar 'B' = [Long,Short,Short,Short]
encodeChar 'C' = [Long,Short,Long,Short]
encodeChar 'D' = [Long,Short,Short]
encodeChar 'E' = [Short]
encodeChar 'F' = [Short,Short,Long,Short]
encodeChar 'G' = [Long,Long,Short]
encodeChar 'H' = [Short,Short,Short,Short]
encodeChar 'I' = [Short,Short]
encodeChar 'J' = [Short,Long,Long,Long]
encodeChar 'K' = [Long,Short,Long]
encodeChar 'L' = [Short,Long,Short,Short]
encodeChar 'M' = [Long,Long]
encodeChar 'N' = [Long,Short]
encodeChar 'O' = [Long,Long,Long]
encodeChar 'P' = [Short,Long,Long,Short]
encodeChar 'Q' = [Long,Long,Short,Long]
encodeChar 'R' = [Short,Long,Short]
encodeChar 'S' = [Short,Short,Short]
encodeChar 'T' = [Long]
encodeChar 'U' = [Short,Short,Long]
encodeChar 'V' = [Short,Short,Short,Long]
encodeChar 'W' = [Short,Long,Long]
encodeChar 'X' = [Long,Short,Short,Long]
encodeChar 'Y' = [Long,Short,Long,Long]
encodeChar 'Z' = [Long,Long,Short,Short]
encodeChar '-' = [Long] -- hacky
encodeChar '.' = [Short] -- hacky

processRaw :: String -> [String] ->Integer
processRaw morse dictLines = process (encode morse) (sort $ map encode dictLines)

data Tree = Tree {terminations :: Integer, shortTree :: (Maybe Tree), longTree :: (Maybe Tree) } deriving Show

choose :: Beep -> Tree -> Maybe Tree
choose Short = shortTree
choose Long  = longTree

process :: Morse -> [Morse] -> Integer
process morse dict = possibilities fulltree morse [(1,fulltree)]
  where
    fulltree = fromJust $ mkTree dict

possibilities :: Tree -> Morse -> [(Integer,Tree)] -> Integer
possibilities _ [] trees = sum . map (uncurry (*) . fmap terminations) $ trees
possibilities _ _  []    = 0
possibilities fulltree (x:xs) trees = possibilities fulltree xs
                                      (start++catMaybes (map sequenceA $ choose x <<$>> trees))
      where
         tr2 = catMaybes (map sequenceA $ choose x <<$>> trees)
         terminationCount = sum . map (uncurry (*) . fmap terminations) $ tr2
         start = if terminationCount /= 0 then [(terminationCount,fulltree)] else []
         (<<$>>) = fmap . fmap

mkTree :: [Morse] -> Maybe Tree
mkTree [] = Nothing
mkTree suffixes = Just $ Tree endsHere (mkTree s) (mkTree l)
   where
      endsHere = genericLength (filter null suffixes) -- testing for first element should suffice (sorting)
      s        = [ ts | (Short:ts) <- suffixes ]
      l        = [ ts | (Long :ts) <- suffixes ]

