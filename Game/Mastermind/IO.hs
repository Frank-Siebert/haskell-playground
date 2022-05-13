{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}

-- | contains functions for (console) In- and Output
module Game.Mastermind.IO where

import Game.Mastermind.Data
import Control.Monad.RWS( liftIO, ask, get, MonadIO)

-- | The default type for pegs
type Peg = Char

getGuess :: (M Peg m, MonadIO m) => m [Peg]
getGuess = do ruleSet <- ask
              guess <- liftIO getLine
              if length guess /= pegCount ruleSet
                then do liftIO . putStrLn $ "enter "++show (pegCount ruleSet)++" pegs"
                        getGuess
                else if not (all (`elem` colors ruleSet) guess)
                    then do liftIO . putStrLn $ "allowed colors: " ++ colors ruleSet
                            getGuess
                    else do counterExamples <- filter (not . isCandidate guess) <$> get
                            if guessesMustBeSerious ruleSet && not (null counterExamples)
                               then do liftIO . putStrLn $ "guess does not match these previous guesses"
                                       liftIO $ mapM_ (putStrLn . showValuated) counterExamples
                                       getGuess
                               else return guess

showValuation :: Valuation -> String
showValuation v =showAs 'X' blacks ++ showAs 'o' whites
         where showAs c criterion = replicate (criterion v) c

showValuated :: Valuated Peg -> String
showValuated Valuated{valuatedGuess = vg, valuation = v} = vg ++ " " ++ showValuation v
