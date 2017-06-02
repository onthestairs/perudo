module Perudo.Types (
  Dice(..)
, Hand
, PalaficoState(..)
, NumberOfDice
, DiceBet
, Action(..)
, RoundPosition
, PerudoStrategy
, Player(..)
, InRoundPlayer(..)
, PerudoResult
) where

import System.Random

data Dice = One | Two | Three | Four | Five | Six deriving (Show, Enum, Bounded, Eq)
type Hand = [Dice]
data PalaficoState = PalificoDone | PalificoPending deriving (Show, Eq)
type NumberOfDice = Int
type DiceBet = (NumberOfDice, Dice)
data Action = Call | Bet DiceBet deriving (Show, Eq)
type RoundPosition = Int
type PerudoStrategy = Hand -> RoundPosition -> [Action] -> Action
data Player = Player {
    strategy :: PerudoStrategy
  , numberOfDice :: NumberOfDice
  , palifcoState :: PalaficoState
}
instance Show Player where
  show (Player _ numberOfDice palificoState) = "<Player (Dice: " ++ show numberOfDice ++ ") " ++ show palificoState
data InRoundPlayer = InRoundPlayer {
    player :: Player
  , hand :: Hand
  , roundPosition :: RoundPosition
} deriving (Show)
type PerudoResult = Int

instance Random Dice where
  randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of (x, g') -> (toEnum x, g')
  random = randomR (minBound, maxBound)
