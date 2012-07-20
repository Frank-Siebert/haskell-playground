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
