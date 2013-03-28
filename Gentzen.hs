import FsTree
import Data.Function (on)

--data (Eq vars, Show vars) => Expr vars = Var vars
data Expr vars = Var vars
          | Expr vars :===> Expr vars
          | Expr vars :<==> Expr vars
		  | Expr vars :=&&= Expr vars -- TODO abstract binary operators?!
		  | Expr vars :=||= Expr vars
		  | Not (Expr vars)
    deriving (Eq) -- Eq for intersect. Well, this "Var String" thing does not amuse.
    
instance (Show vars) => Show (Expr vars) where
   show (Var v) = show v
   show (Not e)     = "~(" ++ show e ++")"
   show (a :===> b) = "("++show a ++"->" ++show b ++")"
   show (a :<==> b) = "("++show a ++"<-->" ++show b ++")"
   show (a :=&&= b) = "("++show a ++"/\\" ++show b ++")"
   show (a :=||= b) = "("++show a ++"\\/" ++show b ++")"

rank :: Expr a -> Int
rank (Var _) = 0
rank (Not e) = 1 + rank e	
rank (a :===> b) = 1 + max (rank a) (rank b)
rank (a :=&&= b) = 1 + max (rank a) (rank b)
rank (a :=||= b) = 1 + max (rank a) (rank b)
rank (a :<==> b) = 1 + max (rank a) (rank b)
	
--type Line vars = ([Expr vars] , [Expr vars])
data Line vars = [Expr vars] :--- [Expr vars]
instance (Show vars) => Show (Line vars) where
   show (a :--- b) = show a ++ "    |---    " ++ show b
   {-- show (a :--- b) = spaceFill b ++ show a ++ "    |---    " ++ show b where 
     spaceFill [] = "   "
     spaceFill x  = map (const ' ') $ show x --}
   

solve  :: Line vars -> [Line vars]
solve (ante :--- succ) = solve' (sortByRank ante :--- sortByRank succ) -- irgendwo muß das sortieren hin...! Hier isses gut aufgehoben.

solve' :: Line vars -> [Line vars]
solve'    (  []   :---   []  )  = []
solve'  x@( ante  :---   []  )  = solveAnte x
solve'  x@(  []   :---  succ )  = solveSucc x
solve'  x@((a:as) :--- (s:ss))  = if (rank a) > (rank s) then solveAnte x else solveSucc x


solveAnte ((ante:antes) :--- succ) = 
	case ante of
		Var x     -> [] -- find something better!
		Not x     -> [(antes :--- (x:succ))]
		x :===> y -> [(antes :--- (x:succ)), ((y:antes) :--- succ)]
		x :<==> y -> [((x:y:antes) :--- succ),(antes :--- (x:y:succ))]
		x :=&&= y -> [((x:y:antes) :--- succ)]
		x :=||= y -> [((x:antes) :--- succ),((y:antes) :--- succ)]
	
solveSucc (antes  :--- (succ:succs)) = 
	case succ of
		Var x     -> []
		Not x     -> [((x:antes) :--- succs)]
		x :===> y -> [((x:antes) :--- (y:succs))]
		x :<==> y -> [((x:antes) :--- (y:succs)),((y:antes) :--- (x:succs))]
		x :=&&= y -> [(antes :--- (x:succs)),(antes :--- (y:succs))]
		x :=||= y -> [(antes :--- (x:y:succs))]


{- insertAt :: Int -> a -> [a] -> [a]
insertAt 0 el l = el:l
insertAt n el [] = [el]
insertAt n el (x:xs) = x:(insertAt (n-1) el xs) -}

insertAll :: a -> [a] -> [[a]]
insertAll el [] = [[el]]
insertAll el l@(x:xs) = (el:l):(map (x:) (insertAll el xs))

permute :: [a] -> [[a]]
permute [] = [[]]
permute (x:xs) = concat [ insertAll x ys | ys <- permute xs]

sortByRank :: [Expr t] -> [Expr t]
sortByRank = sortBy (flip compare `on` rank)
--sortByRank = sortBy (\a b -> (rank b) `compare` (rank a))

-- Wenn's regnet, ist die Straße nass. Also, wenn die Straße trocken ist, regnet's nicht.

regen :: Expr String
regen = let phi = Var "es regnet"; psi = Var "Strasse nass" in
             ((phi :===> psi) :<==> (Not psi :===> Not phi)) 
			
deMorgan1 = let phi = Var "phi"; psi = Var "psi" in
               (Not (phi :=&&= psi)) :<==> ((Not phi) :=||= (Not psi))
deMorgan2 = let phi = Var "phi"; psi = Var "psi" in
               (Not (phi :=||= psi)) :<==> ((Not phi) :=&&= (Not psi))
			   
isDeduction :: (Eq v) => Line v -> Bool
isDeduction expr = foldTree e (repTree solve expr)
  where e (ante :--- succ) [] = ante `intersect` succ /= []
        e _ subs = and subs
		
tst1, tst2 :: Line String
tst1 = ([Var "a" :<==> Var "b", Var "b" :<==> Var "c"] :--- [Var "a" :<==> Var "c"])
tst2 = ([Var "a" :<==> Var "b", Var "c" :<==> Var "d"] :--- [Var "a" :<==> Var "d"])

deduce :: Expr vars -> Tree (Line vars)
deduce expr = repTree solve ([] :--- [expr])

conn = [(:=&&=),(:=||=),(:===>),(:<==>)]
assoc op = (((Var "a" `op` Var "b") `op` Var "c") :<==> (Var "a" `op` (Var "b" `op` Var "c")))
