-- tries to un-zlib the savegames of wargame: european escalation
-- download haskell platform from http://haskell.org (and install)
-- type 'cabal install Codec.Compression.Zlib' or something (I forgot) at cmd prompt

import qualified Data.ByteString.Lazy as B
import Codec.Compression.Zlib.Raw
import Control.Exception (handle,SomeException)
import System.Environment (getArgs)
import GHC.Int (Int64)

mycompressionLevel n = defaultCompressParams  { compressLevel = CompressionLevel n }

discardException :: Int64 -> IO () -> IO ()
discardException n = handle (\e -> putStr ("Fail at length "++ show n ++ ": " ++ show (e :: SomeException)))

convert :: Int64 -> String -> IO ()
convert n fileName = do 
    inp <- B.readFile fileName
    let header           = B.take n inp
        rawTail          = B.drop n inp
        decompressedTail = decompress rawTail
        targetFile       = fileName ++ ".unpacked"
        outputStream     = B.append header decompressedTail
    B.writeFile targetFile outputStream
    
findZlib :: String -> IO ()
findZlib fileName = do
    inp <- B.readFile fileName
    sequence_ [discardException n (putStr ("\nstarting at " ++ show n ++ ": uncompressed length " ++ (show . length . B.unpack . decompress . B.drop n $ inp))) | n<-[0..255]]

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["u",fileName]        -> convert 74 fileName
        ["findzlib",fileName] -> findZlib fileName
        _ -> putStrLn "usage: wargame u <savegame>"

--    let decompressed = (decompress . B.drop 74 $ inp)
--        recompressed = compressWith (mycompressionLevel 8) decompressed
--        fourZeros    = B.pack ([0,0,0,0])
--        withHeader   = B.append (B.take 74 inp) $ B.append recompressed fourZeros;
--    B.writeFile "E:\\Dokumente und Einstellungen\\Franky\\Eigene Dateien\\EugenSystems\\WarGame\\SavedGames\\defend_10_05_2013_008.repacked.wargamesav" withHeader

data Timed a = Timed {curr :: a, prev :: a}

type Predicate a = a -> Bool

type TimeStrat = Bool -> Bool -> Bool

is :: TimeStrat -> Predicate a -> Timed a -> Bool
is ts pred timed = ts (pred . curr $ timed) (pred . prev $ timed)

now :: TimeStrat
now a _ = a

before :: TimeStrat
before _ b = b

enter :: TimeStrat
enter a b = a && not b

media = Timed 10 1
loading = (==) 10
--is enter loading media
