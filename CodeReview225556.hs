import Control.Concurrent
import Text.Printf

main :: IO ()
main = gameOfLife (
  [ [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ] )

gameOfLife :: [[Integer]] -> IO ()
gameOfLife state = do
  pretty_print state
  let new_state = transition state
  sleep
  printf "\ESC[%dA" $ length state -- move cursor to beginning.
  gameOfLife new_state

pretty_print :: [[Integer]] -> IO ()
pretty_print state =
  mapM_ print_row state
  where
    print_row linestate =
      putStrLn $ map sprite linestate

sprite :: Integer -> Char
sprite 0 = '.'
sprite _ = '*'


-- get new state row by row.
transition :: [[Integer]] -> [[Integer]]
transition state = zipWith3 process_rows (last state:state) state (tail state ++ state)
  where
    process_rows prev row next =
      proc 0
      where
        last_j = ((length row) - 1)
        proc m = proc_col m : (if m == last_j then [] else proc (m + 1))
        proc_col j = live_die (row !! j) (
          -- column to left
            (if j == 0 then (last prev + last row + last next) else
                (prev !! (j - 1) + row !! (j - 1) + next !! (j - 1)))
          -- above & below
          + prev !! j + next !! j
          -- column to right
          + (if j == last_j then (prev !! 0 + row !! 0 + next !! 0) else
                (prev !! (j + 1) + row !! (j + 1) + next !! (j + 1)))
          )

live_die :: Integer -> Integer -> Integer
live_die _ 3 = 1
live_die 1 2 = 1
live_die _ _ = 0

sleep :: IO ()
sleep = threadDelay 100000

zwEx = zipWith3 (\a b c -> a ++ b ++ c)
       ["Hello, ", "Good "]
       ["World"  , "night "]
       ["!"      , "John body"]
