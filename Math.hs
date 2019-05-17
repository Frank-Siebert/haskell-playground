{-# LANGUAGE FlexibleInstances #-}

data Term op n v = Lit n | Var v
              | Terms op [Term op n v] deriving Show

data Operation  = UnNegate | UnAbs | UnSignum | UnRecip
                | MoSum | MoProduct
                | BoDifference | BoQuotient deriving (Eq,Show)

isAssoc :: Operation -> Bool
isAssoc MoSum = True
isAssoc MoProduct = True
isAssoc _ = False

opFunc :: (Fractional n) => Operation -> [n] -> n
opFunc UnNegate     = negate . head
opFunc UnAbs        = abs . head
opFunc UnSignum     = signum . head
opFunc MoSum        = sum
opFunc MoProduct    = product
opFunc BoDifference = unc (-)
opFunc BoQuotient   = unc (/)

unc op [a,b] = a `op` b

type TermStdOp = Term Operation

instance Num n => Num (Term Operation n v) where
   fromInteger n = Lit (fromInteger n)
   negate n = Terms UnNegate [n]
   abs n = Terms UnAbs [n]
   signum n = Terms UnSignum [n]
   a + b = Terms MoSum [a,b]
   a - b = Terms BoDifference [a,b]
   a * b = Terms MoProduct [a,b]

instance Fractional n => Fractional (Term Operation n v) where
   a / b = Terms BoQuotient [a,b]
   recip a = Terms UnRecip [a]
   fromRational a = Lit (fromRational a)

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
takeVars (Lit _) = []
takeVars (Terms _ terms) = concat (map takeVars terms)

insertVar :: (v -> Term op n w) -> Term op n v -> Term op n w
insertVar f (Var v) = f v
insertVar f (Terms op terms) = Terms op (map (insertVar f) terms)
insertVar _ (Lit n) = Lit n

eval :: (Fractional n) => (v->n) -> Term Operation n v -> n
eval f (Var v) = f v
eval _ (Lit n) = n
eval f (Terms op terms) = opFunc op (map (eval f) terms)
