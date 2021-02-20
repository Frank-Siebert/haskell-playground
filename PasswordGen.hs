import Data.List(genericIndex,genericLength)

type I = Integer -- Int or Integer? Decide later!
data Coding = Coding I I deriving Show

myChars :: [Char]
myChars = ['a'..'z']++['A'..'Z']++['0'..'9']++",.!$%"

myCharCount :: I
myCharCount = genericLength myChars

data Output c = Output (I -> c) I

myOutput :: Output Char
myOutput = Output (genericIndex myChars) (genericLength myChars)

genPwCoding :: Output c -> [I] -> [(Coding,[c])]
genPwCoding (Output charSelect charCount) = go (Coding 0 1) where
  go coding@(Coding low range) i
    | low * myCharCount `quot` range == (low+1) * charCount `quot` range
      = (coding,[charSelect (low * myCharCount `quot` range)]) : go (Coding (low `quot` charCount) (range `quot` charCount)) i
    | (h:t)<-i  = (coding,[]) : go (Coding (low*6+h) (range*6)) t
    | otherwise = [(coding,[])]
   