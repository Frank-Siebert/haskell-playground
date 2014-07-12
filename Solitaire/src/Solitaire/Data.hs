module Solitaire.Data where

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Show,Read,Eq,Enum,Bounded)
data Rank = Ace | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | Jack | Queen | King deriving (Show,Read,Eq,Enum,Ord,Bounded)
data Card = Rank :/ Suit deriving (Show,Read,Eq)

type Cards = [Card]
data Column = Column [Card] [Card] deriving (Show)-- hidden, open
type Table = [Column]
-- TODO undealt cards.

data GameState = GameState {
                     table :: Table,
                     undealt :: Cards } deriving (Show)
