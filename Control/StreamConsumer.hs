{-# LANGUAGE DeriveFunctor #-}
module Control.StreamConsumer
  ( StreamConsumer(..)
  , evalStreamConsumer
  , pop
  ) where

import Control.Applicative(Alternative,empty,(<|>))

-- more a Generator than a consumer
newtype StreamConsumer f s a = StreamConsumer { runStreamConsumer :: s -> (s,f a)} deriving Functor

-- don't know if I could just use the State Monad
instance (Applicative f) => Applicative (StreamConsumer f s) where
    pure x = StreamConsumer $ \s -> (s,pure x)
    StreamConsumer scf <*> StreamConsumer scx =  StreamConsumer $ \s ->
        let (s' ,f) = scf s
            (s'',x) = scx s'
         in (s'',f <*> x)

instance (Alternative f) => Alternative (StreamConsumer f s) where
    empty = StreamConsumer $ \s -> (s,empty)
    StreamConsumer scx <|> StreamConsumer scy = StreamConsumer $ \s ->
        let (s' ,x) = scx s
            (s'',y) = scy s'
         in (s'',x <|> y)

evalStreamConsumer :: s -> StreamConsumer f s a -> f a
evalStreamConsumer s (StreamConsumer sc) = snd $ sc s

pop :: (Applicative f) => StreamConsumer f [a] a
pop = StreamConsumer $ \(x:xs) -> (xs,pure x)
