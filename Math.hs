{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

data Term op n v = Lit n | Var v
              | Terms op [Term op n v] deriving (Show,Functor)

data Operation  = UnNegate | UnAbs | UnSignum | UnRecip
                | MoSum | BoDifference
                | MoProduct | BoQuotient deriving (Eq,Show,Ord)

isAssoc :: Operation -> Bool
isAssoc MoSum = True
isAssoc MoProduct = True
isAssoc _ = False

opFunc :: (Fractional n) => Operation -> [n] -> n
opFunc UnNegate     = negate . head
opFunc UnAbs        = abs . head
opFunc UnSignum     = signum . head
opFunc UnRecip      = recip . head
opFunc MoSum        = sum
opFunc MoProduct    = product
opFunc BoDifference = unc (-)
opFunc BoQuotient   = unc (/)


unc :: (a -> a -> a) -> [a] -> a
unc op [a,b] = a `op` b
unc _  _     = error "unc: list should have exactly two arguments"

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

instance Applicative (Term op n) where
   pure = Var
   a <*> b = insertVar (\f -> insertVar (Var . f) b) a

instance Monad (Term op n) where
   return = pure
   (>>=) = flip insertVar

instance Foldable (Term op n) where
   foldr f acc (Lit _) = acc
   foldr f acc (Var x) = f x acc
   foldr f acc (Terms _ []) = acc
   foldr f acc (Terms op (x:xs)) = foldr f (foldr f acc x) (Terms op xs)
   foldMap _ (Lit _) = mempty
   foldMap f (Var x) = f x
   foldMap f (Terms _ terms) = foldMap (foldMap f) terms

instance Traversable (Term op n) where
   traverse f (Lit n) = pure (Lit n)
   traverse f (Var x) = Var <$> f x
   traverse f (Terms op terms) =  Terms op <$> traverse (traverse f) terms

mapOperation :: (a -> b) -> Term a n v -> Term b n v
mapOperation f t = go t where
                   go (Terms a terms) = Terms (f a) (map go terms)
                   go (Lit n) = Lit n
                   go (Var v) = Var v

eval :: (v->n) -> Term ([n] -> n) n v -> n
eval f (Var v) = f v
eval _ (Lit n) = n
eval f (Terms op terms) = op (map (eval f) terms)

evalFractional :: (Fractional n) => (v->n) -> Term Operation n v -> n
evalFractional f = eval f . mapOperation opFunc

