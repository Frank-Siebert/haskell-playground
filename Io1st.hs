module Io1st where

main1st :: IO ()
main1st = do
          putStr "Dann gib mal was ein? " 
          x <- getLine 
          putStrLn ("Eingabe war \""++x++"\"") 
          putStrLn "Terminiere."

maini :: IO ()
maini = interact id

main :: IO ()
main = do
            x <- getLine  
            print ((read x) +2)


data State = State Int
getValue :: State -> Int
getValue (State x) = x

looping :: State -> IO State
looping inState =
           do input <- getLine
              if getValue inState > toInt input 
                then putStrLn ("Zu gross!")
                else putStrLn ("Kleiner oder gleich")
              return inState
                
            
toInt :: String -> Int
toInt = read

-- copied from http://www.haskell.org/haskellwiki/IO_inside . I doubt it compiles.
when :: Bool -> IO () -> IO a -> IO a
when condition action world =
    if condition
      then action >> world
      else world

-- my guesses
--while :: (IO t -> Bool) -> IO t -> IO t
--while :: (t -> Bool) -> IO t -> IO t
