-- takes base and number and produces base-adic list,
-- e.g. toList 10 42 = [4,2]
--      toList 2 14 = [1,1,1,0]
toList :: Integer -> Integer -> [Integer]
toList = (reverse .) . toList' where
  toList' _ 0 = []
  toList' base num = (num `mod` base) : toList' base (num `div` base) 

-- reverse of above.
fromList :: Integer -> [Integer] -> Integer
fromList base = foldl ((+) . (* base) ) 0

-- the possible characters for passwords
myChars = ['a'..'z']++['A'..'Z']++['0'..'9']++",.!$%"

-- for mapping a number to password
toChars = (!!) myChars . fromInteger

charCount = toInteger $ length myChars

-- takes a list of dice rolls (range 1 to 6) and returns a string for password.
makePW' :: [Integer] -> String
makePW' = map toChars . toList charCount . fromList 6 . map (subtract 1)

-- takes an Integer with digits 1 to 6 and returns a string for password.
makePW :: Integer -> String
makePW = makePW' . toList 10 

data Password = Password { pw :: String, x :: Int, total :: Int }
-- total is the number of possibilities for x, 0<=x<total, so init with 1
--instance Monad Password where
--    return
data PasswordGen = PasswordGen { chars :: [Char], inputmax:: Int }

addDice :: Int -> PasswordGen -> Password -> Password
addDice dice (PasswordGen chars inputmax) (Password pw x total) = Password pw (x*inputmax+dice) (total * inputmax)


{--
generatePw :: String -> PasswordGen -> Password
generatePw' :: Int -> Password -> Password
generatePw' dice pwd = let base = length (chars pwd) in
                         if total pwd * inputmax >= base
                          then Password { pw = char gen !! x pwd `mod` base : pw pwd, x = x pwd `div` base, total = total }
						  else Password { pw = pw pwd, x = x pwd * chars pwd + dice, total = total pwd * inputmax }
						  --}

data State = State Int Int
emit :: PasswordGen -> State -> (State, String)
emit (PasswordGen chars sidedness) (State x total) = ((State x total),"")