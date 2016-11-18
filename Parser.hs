{-# LANGUAGE DeriveFunctor #-}

import Control.Applicative (Alternative,empty,(<|>),some,liftA2)
import Control.Monad (MonadPlus,mzero,mplus)
import Data.Char (isDigit)
import Data.List (isPrefixOf, span)

newtype Parser t a = Parser { runParser :: [t] -> [([t],a)] } deriving Functor

instance Applicative (Parser t) where
    pure x =  Parser (\s -> [(s,x)])
    pf <*> px = Parser $ \s1 ->
            concat [[(s3,f x) | (s3,x) <- runParser px s2] | (s2,f) <- runParser pf s1]
            
instance Alternative (Parser t) where
    empty = Parser (const [])
--    pa <|> pb = Parser (\s -> runParser pa s ++ runParser pb s)
    Parser a <|> Parser b = Parser (\s -> case a s of
                []   -> b s
                more -> more)

instance Monad (Parser t) where
    return = pure
    p >>= f = Parser $ \s1 ->
            concat [runParser (f x) s2 | (s2,x) <- runParser p s1]

instance MonadPlus (Parser t) where
    mzero = empty
    mplus = (<|>)


raw' :: (Eq t) => t -> Parser t ()
raw' c = Parser $ (\s -> case s of
              (x:xs) | x == c -> [(xs,())]
              _               -> [])

raw :: (Eq t) => [t] -> Parser t ()
raw c = Parser $ \s -> if c `isPrefixOf` s then [(drop (length c) s,())] else []

one :: Parser t t
one = Parser $ f where
        f [] = []
        f (x:xs) = [(xs,x)]

pfilter :: (t -> Bool) -> Parser t t
pfilter predicate = Parser $ f where
        f [] = []
        f (x:xs) | predicate x = [(xs,x)]
                 | otherwise   = []

parseWhile :: (t -> Bool) -> Parser t [t]
parseWhile p = Parser $ \s-> case span p s of
    ([], _)   -> []
    (match,r) -> [(r,match)]
                 
number :: Parser Char Int
number = read <$> parseWhile (isDigit)

try :: Parser t a -> Parser t a
try (Parser p) = Parser $ \s -> let rs = p s in if map fst rs
                                    
 

data Exp = N Int | Exp :+ Exp deriving (Show)

parseExp :: Parser Char Exp
parseExp = try raw' '(' *> parseExp <* raw' ')'
       <|> (parseExp <* raw' '+') <:+> parseExp
       <|> N <$> number
       where (<:+>) = liftA2 (:+)
-- stack overflow or does not parse "1+2" if lines <:+> and N <$> are swapped
