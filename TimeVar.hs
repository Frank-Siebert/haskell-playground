{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}

import Control.Applicative

data TimeVar e a = TV (e -> (a,TimeVar e a))
--data TimeVar e a = TV (e -> (Either a e, TimeVar e a))
--data TimeVar e a = TV (e -> (Maybe a, Maybe e, TimeVar e a))

-- this is not a tick-based blah-foo!
--data TV2 a = Source (a, TV2 a) | Sink (TV2 a)

deriving instance Functor (TimeVar e)

instance Applicative (TimeVar e) where
   pure x = let z = TV (\_ -> (x,z)) in z
   TV f <*> TV x = TV $ \e ->
        let
            (f',tvf) = f e
            (x',tvx) = x e
         in
            (f' x',tvf <*> tvx)

{-
instance Monad (TimeVar e) where
    return = pure
    TV x >>= f = TV $ \e ->
        let
            (x',tvx) = x e
            (y ,tvy) = f x'
         in
            (undefined, tvx >>= f)
-}

acc :: (a -> a) -> a -> TimeVar e a
acc step init = TV $ \_ -> (init, acc step (step init))

testTV :: TimeVar e a -> [e] -> [a]
testTV    _     []   = []
testTV (TV f) (e:es) = let (x,tv) = f e in x:testTV tv es

--numsi = acc (+1) 0

--addsi = (+) <$> numsi <*> parrot -- pull!

-- TimeVar DSI LockState, TimeVar DSI ActivationState
-- kein Problem mit DSI = LockState | ActivationState
-- wie werden die Events eingespielt, bei allen "gleichzeitig"?
-- es muss "push-based" sein?

parrot :: TimeVar e e
parrot = TV (\e -> (e,parrot))
