-- re-implementation of the Cofree Comand, just for learning.

import Control.Comonad

data Cofree f a = Cofree a (f (Cofree f a))

instance (Functor f) => Functor (Cofree f) where
  fmap f (Cofree x cof) = Cofree (f x) (fmap (fmap f) cof)

instance (Functor f) => Comonad (Cofree f) where
  extract (Cofree x _) = x
  duplicate z@(Cofree x cof) = Cofree z (fmap duplicate cof)

-- that is not worth saving it, is it?
-- well, some more

toList :: Cofree Maybe a -> [a] -- non-empty list, that is.
toList (Cofree x Nothing) = [x]
toList (Cofree x (Just xs)) = x:toList xs
