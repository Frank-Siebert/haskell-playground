data Token = OpenParen
           | CloseParen
           | Operator Char
           | PosNum Double
     deriving (Show, Eq)

data ParseTree = NumNode Double
               | OpNode Char [ParseTree]
         deriving Show

scan :: String ->[Token]
scan xs
     | null lexeme = []
     | c == '(' = OpenParen:scan rest
     | c == ')' = CloseParen:scan rest
     | elem c ['+', '-', '*', '/'] = Operator c: scan (cs ++ rest)
     | elem c ['0'..'9'] = PosNum (read (c:cs):: Double):scan rest
     | otherwise = error ("Lexical Error - invalid character: " ++ [c])
     where [(lexeme, rest)] = lex xs
           c:cs = lexeme

recognize :: [Token] -> Bool
recognize ts = let (s, r) = rexpr ts
     in s && null r

-- <expr> ->OPENPAREN OPERATOR <operands> CLOSEPAREN |  POSNUMBER
rexpr :: [Token] -> (Bool, [Token])
rexpr (OpenParen:Operator  _: rest) = let(b1,r1) = roperands rest
     in case r1 of
          CloseParen:r2 -> (b1, r2)
          _ ->  error "Parse Error: expected closing parenthesis"
          
rexpr (PosNum _:rest) = (True, rest)
rexpr inv = error ("Parse Error: invalid expression " ++ show inv)

-- <operands> ->  <expr> [<operands>]
roperands :: [Token] -> (Bool, [Token])
roperands ts = let (b1, r1) = rexpr ts
     in case isValidStart r1 of
          True -> let (b2, r2) = roperands r1 in (b1 && b2, r2)
          False     -> (b1, r1)

isValidStart :: [Token] -> Bool
isValidStart (OpenParen: _) = True
isValidStart (PosNum _:_) = True
isValidStart _ = False

check:: String -> Bool
check = recognize.scan