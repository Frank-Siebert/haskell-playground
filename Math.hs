{-# LANGUAGE FlexibleInstances #-}

data Term uo bo mo n v = Lit n | Var v
              | UnaryTerm uo (Term uo bo mo n v)
              | BinaryTerm bo (Term uo bo mo n v) (Term uo bo mo n v)
              | MultiTerm mo [Term uo bo mo n v] deriving Show

-- data NamedOp = SumOp | DiffOp | FactorOp | QuotientOp | ExpOp | NegOp
data UnaryOperations = UnNegate | UnAbs | UnSignum deriving (Eq,Show)
data MultiOperations = MoSum | MoProduct deriving (Eq,Show)
data BinaryOperations = BoDifference | BoQuotient deriving (Eq,Show)

type TermStdOp = Term UnaryOperations BinaryOperations MultiOperations

instance Num n => Num (Term UnaryOperations BinaryOperations MultiOperations n v) where
   fromInteger n = Lit (fromInteger n)
   negate n = UnaryTerm UnNegate n
   abs n = UnaryTerm UnAbs n
   signum n = UnaryTerm UnSignum n
   a + b = MultiTerm MoSum [a,b]
   a - b = BinaryTerm BoDifference a b
   a * b = MultiTerm MoProduct [a,b]

x :: TermStdOp n String
x = Var "x"

collectAssoc :: (Eq mo) => Term uo bo mo n v -> Term uo bo mo n v
collectAssoc (MultiTerm op terms) = MultiTerm op (concat (map embed terms)) where
   embed (MultiTerm op' terms') | op == op' = concat (map embed terms')
                                | otherwise = [collectAssoc (MultiTerm op' (map collectAssoc terms'))]
   embed (BinaryTerm op' t1 t2 ) = [BinaryTerm op' (collectAssoc t1) (collectAssoc t2)]
   embed (UnaryTerm op' term) = [UnaryTerm op' (collectAssoc term)]
   embed other = [other]
collectAssoc (UnaryTerm op term) = UnaryTerm op (collectAssoc term)
collectAssoc (BinaryTerm op t1 t2) = BinaryTerm op (collectAssoc t1) (collectAssoc t2)
collectAssoc other = other

takeVars :: Term ou bo mo n v -> [v]
takeVars (Var v) = [v]
