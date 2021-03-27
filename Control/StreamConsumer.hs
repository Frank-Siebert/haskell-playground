{-# LANGUAGE DeriveFunctor #-}
module Control.StreamConsumer
  ( StreamConsumer'(..)
  , StreamConsumer
  , evalStreamConsumer
  , pop
  , consume
  ) where

import Control.Applicative
import Control.Monad(join)

-- goal of this experiment was to show / check if OneTwo is a Monad and under which functions, and...
-- ...another goal was to generate examples.
newtype StreamConsumer' f s a = StreamConsumer { runStreamConsumer :: s -> (s,f a)} deriving Functor
type StreamConsumer = StreamConsumer' []

-- don't know if I could just use the State Monad
instance (Applicative f) => Applicative (StreamConsumer' f s) where
    pure x = StreamConsumer $ \s -> (s,pure x)
    StreamConsumer scf <*> StreamConsumer scx =
             StreamConsumer $ \s -> let (s',f)  = scf s
                                        (s'',x) = scx s'
                                     in (s'',f <*> x)

instance (Alternative f) => Alternative (StreamConsumer' f s) where
    empty = StreamConsumer $ \s -> (s,empty)
    StreamConsumer x <|> StreamConsumer y = StreamConsumer $ \s ->
        let (s' ,xs) = x s
            (s'',ys) = y s'
         in (s'',xs <|> ys)

evalStreamConsumer :: s -> StreamConsumer s a -> [a]
evalStreamConsumer s (StreamConsumer sc) = snd $ sc s

pop :: StreamConsumer [a] a
pop = StreamConsumer $ \(x:xs) -> (xs,[x])

consume :: (s -> [a]) -> StreamConsumer [s] a
consume f = StreamConsumer $ \(x:xs) -> (xs,f x)
