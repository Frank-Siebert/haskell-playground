{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.RWS(MonadIO, liftIO, get, modify, unless, runRWST)
import Control.Monad.Writer ( MonadWriter )
import Data.Foldable ( maximumBy )
import Data.List ( maximumBy, unfoldr )
import Data.Ord ( comparing )
import System.Random ( getStdGen, uniformR )

import Game.Mastermind.Data
import Game.Mastermind.IO

randomTarget :: (MonadIO m) => RuleSet a -> m (Guess a)
randomTarget ruleSet = do g <- liftIO getStdGen
                          let r = take (pegCount ruleSet) . unfoldr (Just . uniformR (0, length (colors ruleSet) - 1)) $ g
                          return $ map (colors ruleSet !!) r

play :: (M Peg m, MonadIO m, MonadWriter () m) => m ()
play        = do possibilities <- getPossibilities
                 let guessCount = length possibilities
                 let guessDisp = if guessCount < 12 then unwords possibilities else show guessCount
                 liftIO . putStrLn $ "Guess! Remaining Possibilities: " ++ guessDisp
                 guess <- getGuess
                 hist <- get
                 let remainingTargets = [(length . filter (`isCandidate` v) $ possibilities, p) | p <- possibilities, let v = Valuated guess (valuate p guess)]
                 -- liftIO $ mapM_ print remainingPoss
                 let (remainingCount,evilTarget) = maximumBy (comparing fst) remainingTargets
                 -- liftIO . print $ z
                 let v = Valuated {valuatedGuess = guess, valuation = valuate evilTarget guess}
                 modify (v:)
                 liftIO . putStrLn $ showValuated v
                 unless (remainingCount == 1) play

defaultRuleSet :: RuleSet Peg
defaultRuleSet = RuleSet {
    pegCount             = 4,
    colors               = ['1'..'6'],
    duplicatesAllowed    = True,
    guessesMustBeSerious = False
}

main :: IO ()
main = do runRWST play defaultRuleSet []
          return ()
