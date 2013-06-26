data Chain a = Chain String a deriving (Eq,Show)

instance (Monad) (Chain) where
   return x = Chain "" x
   Chain s x >>= f = let (Chain s' x') = (f x) in Chain (s++s') x'
   Chain s _ >> Chain s' x' = Chain (s++s') x'

pr :: (Show a) => a -> Chain a
pr x = Chain (show x) x

ps :: String -> Chain String
ps s = Chain s s

pf :: (Show b) => (a -> b) -> a -> Chain b
pf f x = let y = f x in Chain (show y) y


init :: Chain ()
init = Chain "" ()

instance Functor Chain where
  fmap f (Chain s x) = (Chain s (f x))

data Intree a = Nil | Node (Intree a) a (Intree a)

instance Functor Intree where
  fmap f Nil = Nil
  fmap f (Node left x right) = Node (fmap f left) (f x) (fmap f right)
  
instance Monad Intree where
  return x = Node Nil x Nil
  Nil >>= f = Nil
  (Node left x right) >>= f = case f x of
      Nil -> Nil -- what about left and right?
      (Node left' y right') -> Node (left >>= f) y right'  -- todo! what about left' and right?
      
data IIO a = IIO (IO a) a
instance Monad (IIO) where
  return x = IIO (return x) x
--  (IIO io x) >>= f = let (IIO io' x') = f x in IIO (io >> putStrLn "interject"  >> io') x'
  (IIO io x) >>= f = let (IIO io' x') = f x in IIO (io >>= (\tmp -> (putStrLn "interject" >> return tmp) ) >> io') x'

runIIO (IIO io x) = io
r :: IO () -> IIO ()
r io = IIO io ()

--rf :: a -> (a -> IO b) -> IIO b
--rf x f = IIO (a >> 


-- works:
-- runIIO $ IIO (putStrLn "1") () >> r (putStrLn "2") >> r (putStrLn "3")
-- runIIO $ IIO (putStrLn "1") () >> return "Hallo" >>= r . putStrLn 

-- a: final type. b: intermediate (monad) type?
-- need three types for (a->b)->(b->c)->(a->c)
data SIO a b = SIO0 b | SIOplain (b -> IO a) | SIOstored (IO b) (b -> SIO b a) | SIO1 b ( b-> SIO a b)
--SIO1 (c->SIO a) (IO c) |SIO2 (SIO a) b (b -> IO c)

{-
instance Monad (SIO a) where
  return = SIO0
  SIO0 x >> next = next
  SIO0 x >>= f = SIO1 x f
-}

f15 :: (a -> IO b) -> SIO b a
f15 f = SIOplain f

-- a is the type of the final value, and will remain unchanged of course
executeStep :: SIO b a -> IO (SIO b a)
executeStep (SIO0 x) = putStrLn "final step, or do you want to loop forever?" >> return (SIO0 x)
executeStep (SIOstored ioAction follow) = do v <- ioAction
                                             return (SIOstored (return v) (follow))-- und was mit v?
-- fehler hier: der letzte Wert, das innerste muss abgearbeitet werden, nicht von außen nach innen,
-- was weniger garbage erzeugen würde.
-- doch, müsste andersherum gehen, ich setze ja SIO zusammen.

-- what is this "->" in
-- instance Monad (->) or something?
type Function a b = a -> b
-- a type constructor. -> is a type constructor
