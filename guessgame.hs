import System.Random (randomIO)

find :: Int -> Int -> IO String
find target tries | tries > 8 = return "tries exhausted"
                  | otherwise = do 
    putStr $ "Your guess ("++show tries++"): "
    guess <- readLn :: IO Int
    if guess == target then return $ "right after "++show (tries+1)++" tries."
    else do putStrLn $ "too "++ (if guess < target then "low" else "high")
            find target (tries + 1)

main :: IO ()
main = do targetRaw <- randomIO :: IO Int
          --print targetRaw
          let target = targetRaw `mod` 100
          msg <- find target 0
          putStrLn msg
