{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

import Control.Applicative
import Control.Monad(join)
import Control.StreamConsumer
import Data.OneTwo

scOneTwo :: StreamConsumer s a -> StreamConsumer s (OneTwo a)
scOneTwo x = One <$> x <|> Two <$> x <*> x

ot3 :: [OneTwo (OneTwo (OneTwo Int))]
ot3 = eval . scOneTwo . scOneTwo . scOneTwo $ pop

-- helper for repl only
eval :: StreamConsumer [Int] a -> [a]
eval = evalStreamConsumer [1..]

consume :: (s -> [a]) -> StreamConsumer [s] a
consume f = StreamConsumer $ \(x:xs) -> (xs,f x)

-- following functions were part of the journey / exploration.
foo :: (forall z . [z] -> [OneTwo z]) -> a -> [OneTwo (OneTwo (OneTwo a))]
foo f a = (f . f . f) [a]

bar :: (forall z . [z] -> [OneTwo z]) -> a -> [(OneTwo (OneTwo a))]
bar f a = (f . f) [a]

baz :: [a] -> [OneTwo a]
baz x = map One x ++ liftA2 Two x x

ot3u = foo baz ()


checkReturn :: (Eq (m a),Show (m a),Functor m) => (forall a. (a -> m a)) -> (m (m a) -> m a) -> [m a] -> [String]
checkReturn r j ms = [ "j . r violated for "     ++show m | m <-ms, j (r  $  m) /= m]
                  ++ [ "j . fmap r violated for "++show m | m <-ms, j (r <$> m) /= m]

checkJoin :: (Eq (m a),Show (m (m (m a))),Functor m) => (forall a. m (m a) -> m a) -> [m (m (m a))] -> [String]
checkJoin j ms = [ "j . j violated for "     ++show m | m <-ms, j (j m) /= j (j <$> m)]

joinOne :: OneTwo (OneTwo a) -> OneTwo a
joinOne (One x) = x
joinOne (Two x y) = Two (left x) (right y)

joinTwo :: OneTwo (OneTwo a) -> OneTwo a
joinTwo (Two (Two x _) (Two _ y)) = Two x y
joinTwo x = One . right . right $ x
{-
ok:
checkReturn (One) joinOne [One 1, Two 2 3]
checkReturn (join Two) joinTwo [One 1, Two 2 3]
checkJoin joinOne $ ot3u


nok
checkReturn (join Two) joinOne [One 1, Two 2 3]
checkReturn (One) joinTwo [One 1, Two 2 3]
putStrLn $ unlines $ checkJoin joinTwo $ ot3u
j . j violated for Two (Two (Two () ()) (One ())) (Two (One ()) (Two () ()))
j . j violated for Two (Two (Two () ()) (One ())) (Two (Two () ()) (Two () ()))
j . j violated for Two (Two (Two () ()) (Two () ())) (Two (One ()) (Two () ()))
-}
