{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

import Control.Applicative(Alternative,(<|>))
import Control.Monad(join)
import Control.StreamConsumer
import Data.OneTwo

ot1 :: [OneTwo Int]
ot1 = [One 1, Two 2 3]

scOneTwo :: (Alternative f) => StreamConsumer f s a -> StreamConsumer f s (OneTwo a)
scOneTwo x = One <$> x <|> Two <$> x <*> x

ot3 :: [OneTwo (OneTwo (OneTwo Int))]
ot3 = eval . scOneTwo . scOneTwo . scOneTwo $ pop

eval :: StreamConsumer [] [Int] a -> [a]
eval = evalStreamConsumer [1..]

checkReturn :: (Eq (m a),Show (m a),Functor m) => (forall z. (z -> m z)) -> (m (m a) -> m a) -> [m a] -> [String]
checkReturn r j ms = [ "j . r violated for "     ++show m | m <-ms, j (r  $  m) /= m]
                  ++ [ "j . fmap r violated for "++show m | m <-ms, j (r <$> m) /= m]

checkJoin :: (Eq (m a),Show (m (m (m a))),Functor m) => (forall z. m (m z) -> m z) -> [m (m (m a))] -> [String]
checkJoin j ms = [ "j . j violated for "     ++show m | m <-ms, j (j m) /= j (j <$> m)]

joinOne :: OneTwo (OneTwo a) -> OneTwo a
joinOne (One x) = x
joinOne (Two x y) = Two (left x) (right y)

joinTwo :: OneTwo (OneTwo a) -> OneTwo a
joinTwo (Two (Two x _) (Two _ y)) = Two x y
joinTwo x = One . right . right $ x

main :: IO ()
main = do putStrLn "These turned out to be OK:"
          putStrLn          "checkReturn One joinOne $ ot1"
          putStr . unlines . checkReturn One joinOne $ ot1
          putStrLn          "checkReturn (join Two) joinTwo $ ot1"
          putStr . unlines . checkReturn (join Two) joinTwo $ ot1
          putStrLn          "checkJoin joinOne $ ot3"
          putStr . unlines . checkJoin joinOne $ ot3
          putStrLn "Those turned out to be NOT OK:"
          putStrLn          "checkReturn (join Two) joinOne $ ot1"
          putStr . unlines . checkReturn (join Two) joinOne $ ot1
          putStrLn          "checkReturn (One) joinTwo $ ot1"
          putStr . unlines . checkReturn (One) joinTwo $ ot1
          putStrLn          "checkJoin joinTwo $ ot3"
          putStr . unlines . checkJoin joinTwo $ ot3
