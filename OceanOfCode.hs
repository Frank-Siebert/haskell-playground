import System.IO
import Control.Monad

type Cell = (Int,Int) -- will certainly be changed later
type Sector = Int

sectorOfCell :: Cell -> Sector
sectorOfCell (x,y) = 1 + x `rem` 5 + (y `rem` 5)*3

data Board =
     Board { water :: Cell -> Bool
           }

data Intelligence = Intelligence
    { canBe :: Cell -> Bool
    }

data Captain = Captain
    { isSafe :: Cell -> Bool
    }

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.

    input_line <- getLine
    let input = words input_line
    let width = read (input!!0) :: Int
    let height = read (input!!1) :: Int
    let myid = read (input!!2) :: Int

    matrix <- replicateM height $ do
        line <- getLine
        return (fmap (=='.') line)
    let board :: Board
        board = Board (\(x,y) -> matrix !! y !! x)

    -- hPutStrLn stderr "Debug messages..."

    -- Write action to stdout
    putStrLn "7 7"

    -- game loop
    forM_ [0..] $ \round -> do
        hPutStrLn stderr $ "round: "++show round
        input_line <- getLine
        let input = words input_line
        hPutStrLn stderr input_line
        let x = read (input!!0) :: Int
        let y = read (input!!1) :: Int
        let mylife = read (input!!2) :: Int
        let opplife = read (input!!3) :: Int
        let torpedocooldown = read (input!!4) :: Int
        let sonarcooldown = read (input!!5) :: Int
        let silencecooldown = read (input!!6) :: Int
        let minecooldown = read (input!!7) :: Int
        input_line <- getLine
        --hPutStrLn stderr $"sonar: "++input_line
        let sonarresult = input_line :: String
        opponentorders <- getLine
        hPutStrLn stderr $ "opponent: "++input_line

        -- hPutStrLn stderr "Debug messages..."

        -- Write action to stdout
        putStrLn $ case round `rem` 6 of
                0 -> "MOVE N TORPEDO"
                1 -> "MOVE E TORPEDO"
                3 -> "MOVE S TORPEDO"
                4 -> "MOVE W TORPEDO"
                _ -> "SURFACE"
