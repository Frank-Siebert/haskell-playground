module Main where

import System.Random(randomRIO)
import System.Exit(exitSuccess)
import Control.Monad(liftM)

vocab     = ["alpha","beta","gamma"]
blacklist = []

pick:: [a] -> IO a --picks random element.
pick x = do i <- randomRIO (0, length x - 1)
            return (x !! i)

type UsedWords = [String]
type Player = Char -> UsedWords -> IO String

main :: IO ()
main = do
  userInput <- getLine
  processUser userInput vocab blacklist

processUser :: String -> [String] -> [String] -> IO a
processUser input vocab blacklist = if input == "quit" then exitSuccess
                                    else do
                                          successor <- getNext input vocab (input:blacklist)
                                          processPC successor vocab blacklist

processPC :: Maybe (IO String) -> [String] -> [String] -> IO a
processPC  Nothing      v b = do putStrLn "I give up"
                                 exitSuccess

processPC (Just ioWord) v b = do word <- ioWord
                                 putStrLn word
                                 userInput <- getLine
                                 processUser userInput v (word:b)

getNext:: String -> [String] -> [String] -> IO (Maybe (IO String))
getNext lastWord vocab blacklist  = do let chooseFrom = filter (`notElem` blacklist) vocab
                                       let matches    = filter (\x -> head x == last lastWord) chooseFrom
                                       if null matches then return Nothing
                                       else return (Just (pick matches) )
