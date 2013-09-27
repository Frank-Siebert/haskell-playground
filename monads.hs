import Control.Monad.Trans (MonadTrans, lift, MonadIO)
import Control.Monad (liftM2)

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

cart :: (Monad m) => m a -> m b -> m (a,b)
cart = liftM2 (,)

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

data My m a = My (m (My m a)) | My0 a

-- lift :: Monad m => m a -> t m a
instance MonadTrans My where
  lift z = My (z >>= return . My0)

--instance (MonadIO m) => MonadIO (My m) where
--  liftIO = lift . liftIO
liftIO :: IO a -> My IO a
liftIO action = My (action >>= return . My0)


instance (Monad m) => Monad (My m) where
  return = My0
  My0 x >>= f = f x
  My action >>= f = My (do x <- action
                           case x of 
                              My0 x' -> return . f $ x'
                              z -> return (z >>= f) )


executeMyStep :: My m a -> m (My m a) -- ????
executeMyStep (My x) = x

io :: a -> IO a
io = return

steps = do My0 ()
           liftIO $ print "Hallo"
           liftIO $ print "World"
steps2 = do My0 ()
            x <- liftIO $ getLine
            liftIO $ putStrLn x
steps1 x = do My0 ()
              liftIO $ print x

repeatM :: (Monad m) => Int -> (a -> m a) -> a -> m a
repeatM 0 _ = return
repeatM 1 f = f
repeatM 2 f = (\x -> f x >>= f)
repeatM n f = (\x -> f x >>= (repeatM (n-1) f))

five'' = \x-> liftIO $ print x >> getLine >>= (\y -> return (x++y))
five' = repeatM 5 five''
five x = My0 x >>= five' >> (liftIO $ return ())

executeStepwise:: (Monad m) => [My m ()] -> m ()
executeStepwise ms = executeStepwise' (ms,[]) >> return () where
    executeStepwise' :: (Monad m) => ([My m ()], [My m ()]) -> m ([My m ()], [My m ()])
    executeStepwise' ([],[]) = return ([],[])
    executeStepwise' ([], ms') = executeStepwise' (reverse ms',[])
    executeStepwise' ((My0 ()):ms, ms') = executeStepwise' (ms, ms')
--    executeStepwise' (m:ms, ms') = (do m' <- executeMyStep m
--                                       executeStepwise' (ms, m':ms'))
    executeStepwise' (m:ms, ms') = executeMyStep m >>= executeStepwise' . (,) ms . (:ms')
-- todo: still swaps order, so we get 1, 1', 2', 2, 3, 3',...
-- use something more elegant than reverse

threads :: [My IO ()]
threads = [sequence_ [liftIO (putStrLn $ "Thread "++(c:" step "++ show n)) | n<-[1..4]] | c <- ['A'..'D']]
