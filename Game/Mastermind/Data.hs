{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Game.Mastermind.Data where

import Control.Monad (when,unless,mapM_, replicateM)
import Control.Monad.Reader (liftIO,MonadReader,MonadIO,ask)
import Control.Monad.State (MonadState,get,modify)
import Control.Monad.Writer( MonadWriter )
import Data.List( group, sort, unfoldr )

type Guess p = [p]
-- | I cannot decide the constraints for Pegs up front, defer with this definition.
type Peggy p = (Ord p) -- vielleicht reicht auch Eq
type Ruled p m = (MonadReader (RuleSet p) m)
type Historized p m = (MonadState [Valuated p] m)
type M p m = (Ruled p m, Historized p m)

-- | the result of a guess
data Valuation = Valuation {
    blacks        :: Int,
    whites        :: Int
} deriving (Eq,Show)

-- | a line of play "(Red)(Green)(Blue)(Red) Xoo"
data Valuated a = Valuated {
    valuatedGuess :: Guess a,
    valuation     :: Valuation
}

data RuleSet p = RuleSet {
    pegCount             :: Int,
    colors               :: [p],
    duplicatesAllowed    :: Bool,
    -- ^ whether the target allows duplicate colors
    guessesMustBeSerious :: Bool
    -- ^ guesses that cannot fulfill the previous Valuations are rejected
}

isCandidate :: (Peggy p) => Guess p -> Valuated p -> Bool
isCandidate g (Valuated vg v) = v == valuate vg g

-- | is commutative, actually!
valuate :: (Peggy p) => Guess p -> Guess p -> Valuation
valuate target guess = Valuation {
        blacks = samePos,
        whites = anyPos - samePos } where
    samePos = sum [ 1 | (t,g) <- zip target guess, t==g]
    countList :: (Ord z) => [z] -> [(z,Int)]
    countList = map (\xs -> (head xs, length xs)) . group . sort
    target' = countList target
    guess'  = countList guess
    anyPos  = sum [ maybe 0 (min n) (lookup c guess') | (c,n) <- target']

enumerateAll :: (Ruled p m) => m [Guess p]
enumerateAll = do ruleSet <- ask
                  return $ replicateM (pegCount ruleSet) (colors ruleSet)
-- definition below works, but overdoes it!                  
-- enumerateAll = asks $ liftA2 replicateM pegCount colors 
-- todo: does not respect 'allowDuplicates ruleSet'

getPossibilities :: (Peggy p, Ruled p m, Historized p m) => m [Guess p]
getPossibilities = do history <- get
                      filter (\c -> all (isCandidate c) history) <$> enumerateAll
