{-# LANGUAGE FlexibleInstances #-}

data Term op n v = Lit n | Var v
              | Terms op [Term op n v] deriving Show

data Operation  = UnNegate | UnAbs | UnSignum
                | MoSum | MoProduct
                | BoDifference | BoQuotient deriving (Eq,Show)

isAssoc :: Operation -> Bool
isAssoc MoSum = True
isAssoc MoProduct = True
isAssoc _ = False

type TermStdOp = Term Operation

instance Num n => Num (Term Operation n v) where
   fromInteger n = Lit (fromInteger n)
   negate n = Terms UnNegate [n]
   abs n = Terms UnAbs [n]
   signum n = Terms UnSignum [n]
   a + b = Terms MoSum [a,b]
   a - b = Terms BoDifference [a,b]
   a * b = Terms MoProduct [a,b]

x :: TermStdOp n String
x = Var "x"

collectAssoc :: (Eq op) => (op -> Bool) -> Term op n v -> Term op n v
collectAssoc assoc (Terms op terms) | assoc op = Terms op (concat (map embed terms))
                              | otherwise  = Terms op (map (collectAssoc assoc) terms) where
   embed (Terms op' terms') | op == op' = concat (map embed terms')
                            | otherwise = [collectAssoc assoc (Terms op' (map (collectAssoc assoc) terms'))]
   embed other = [other]
collectAssoc _ other = other

takeVars :: Term op n v -> [v]
takeVars (Var v) = [v]
