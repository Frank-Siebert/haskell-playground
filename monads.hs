import Control.Monad.Trans (MonadTrans, lift, MonadIO)
import Control.Monad (liftM2)
import Control.Applicative

-- my attempts to record the actions of a monad led to the rediscovery of the free monad

data Chain a = Chain String a deriving (Eq,Show)

instance (Monad) (Chain) where
   return x = Chain "" x
   Chain s x >>= f = let (Chain s' x') = (f x) in Chain (s++s') x'
   Chain s _ >> Chain s' x' = Chain (s++s') x'

instance Applicative Chain where
  pure = Chain ""
  (Chain s f) <*> (Chain s' x) = Chain (s++s') (f x)

-- state just as an exercise
newtype MyState s a = MyState {runState ::s -> (s,a)}

instance Functor (MyState s) where
  fmap f t = MyState $ \s-> let (s', x) = runState t s in (s',f x)

instance Applicative (MyState s) where
  pure x = MyState (\s-> (s,x))
  f <*> x = MyState $ \s-> let (s',f') = runState f s
                               (s'',x') = runState x s'
                            in (s'',f' x')

instance Monad (MyState s) where
  return = pure
  st >>= f = MyState $ \s -> let (s', x) = runState st s in runState (f x) s'

joinState :: MyState s (MyState s a) -> MyState s a
joinState st = MyState $ \s-> let (s', st2)=runState st s in runState st2 s'

-- this was the thing that went through my mind.
type MyState' s a = s -> (a, s)
joinState' :: MyState' s (MyState' s a) -> MyState' s a
joinState' st s = let (x,s') = st s in x s'

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ x = x

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

data Free f a = Return a | Bind (f (Free f a))

-- lift :: Monad m => m a -> t m a
instance MonadTrans Free where
  lift z = Bind (z >>= return . Return)

--instance (MonadIO m) => MonadIO (My m) where
--  liftIO = lift . liftIO
liftIO :: IO a -> Free IO a
liftIO action = Bind (action >>= return . Return)


instance (Functor f) => Monad (Free f) where
  return = Return
  Return x    >>= f = f x
  Bind action >>= f = Bind $ fmap (>>= f) action
  
instance (Functor f) => Functor (Free f) where
  fmap f (Return x) = Return (f x)
  fmap f (Bind   x) = Bind $ fmap (fmap f) x

instance (Functor f) => Applicative (Free f) where
  pure = Return
  Return f <*> x = fmap f x
  Bind   f <*> x = Bind $ fmap (<*> x) f

io :: a -> IO a
io = return

steps, steps2 :: Free IO ()
steps = do Return ()
           liftIO $ print "Hallo"
           liftIO $ print "World"
steps2 = do Return ()
            x <- liftIO $ getLine
            liftIO $ putStrLn x
steps1 :: (Show a) => a -> Free IO ()
steps1 x = do Return ()
              liftIO $ print x

repeatM :: (Monad m) => Int -> (a -> m a) -> a -> m a
repeatM 0 _ = return
repeatM 1 f = f
repeatM 2 f = (\x -> f x >>= f)
repeatM n f = (\x -> f x >>= (repeatM (n-1) f))

five'', five' :: String -> Free IO String
five :: String -> Free IO ()
five'' = \x-> liftIO $ print x >> getLine >>= (\y -> return (x++y))
five' = repeatM 5 five''
five x = Return x >>= five' >> (liftIO $ return ())

executeStepwise:: (Monad m) => [Free m ()] -> m ()
executeStepwise monads = executeStepwise' (monads,[]) >> return () where
    executeStepwise' :: (Monad m) => ([Free m ()], [Free m ()]) -> m ([Free m ()], [Free m ()])
    executeStepwise' ([],[]) = return ([],[])
    executeStepwise' ([], ms') = executeStepwise' (reverse ms',[])
    executeStepwise' ((Return ()):ms, ms') = executeStepwise' (ms, ms')
--    executeStepwise' ((My action):ms, ms') = (do m' <- action
--                                       executeStepwise' (ms, m':ms'))
    executeStepwise' ((Bind action):ms, ms') = action >>= executeStepwise' . (,) ms . (:ms')
-- todo: still swaps order, so we get 1, 1', 2', 2, 3, 3',...
-- use something more elegant than reverse

roundRobin :: (Monad m) => [Free m ()] -> m ()
roundRobin [] = return ()
roundRobin (Return    _:ms) = roundRobin ms
roundRobin (Bind action:ms) = do next <- action
                                 roundRobin (ms ++ [next])

threads :: [Free IO ()]
threads = [sequence_ [liftIO (putStrLn $ "Thread "++(c:" step "++ show n)) | n<-[1..4]] | c <- ['A'..'D']]

applThread :: Free IO ()
applThread = lift (putStrLn "Hallo") *> lift (putStrLn "Welt")
