module Perudo.Types (
  Dice(..)
, PlayerId
, Hand
, PalaficoState(..)
, NumberOfDice
, DiceBet(..)
, Action(..)
, RoundPosition
, PerudoStrategy
, Player(..)
, InRoundPlayer(..)
) where

import System.Random

data Dice = One | Two | Three | Four | Five | Six deriving (Show, Enum, Bounded, Eq)
type Hand = [Dice]
data PalaficoState = PalificoDone | PalificoPending deriving (Show, Eq)
type NumberOfDice = Int
data DiceBet = DiceBet NumberOfDice Dice deriving (Show, Eq)
-- diagonalise bets
instance Enum DiceBet where
  toEnum n =
    let (row, r) = quotRem n 11
        (q, r') = quotRem r 5
    in case q of
      2 -> DiceBet (row + 1) One
      _ ->  DiceBet ((2 * row + q) + 1) (toEnum (r' + 1))
  fromEnum (DiceBet n One) = (11 * n) - 1
  fromEnum (DiceBet n d) =
    let row = (n-1) `div` 2
    in case n `mod` 2 of
      0 -> (row*11) + 5 + (fromEnum d) - 1
      1 -> (row*11) + (fromEnum d) - 1
instance Ord DiceBet where
  b1 <= b2 = fromEnum b1 <= fromEnum b2
data Action = Call | Bet DiceBet deriving (Show, Eq)
type RoundPosition = Int
type PerudoStrategy = Hand -> [NumberOfDice] -> RoundPosition -> [Action] -> Action
-- type PerudoStrategy = PlayerId -> Hand -> [ObservedInRoundPlayer] -> [Action] -> Action
type PlayerId = Int
data Player = Player {
    playerId :: PlayerId
  , strategy :: PerudoStrategy
  , numberOfDice :: NumberOfDice
  , palifcoState :: PalaficoState
}
instance Show Player where
  show (Player playerId _ numberOfDice palificoState) = "<Player " ++ show playerId ++ " (Dice: " ++ show numberOfDice ++ ") " ++ show palificoState
data InRoundPlayer = InRoundPlayer {
    player :: Player
  , hand :: Hand
  , roundPosition :: RoundPosition
} deriving (Show)
-- data ObservedInRoundPlayer = ObservedInRoundPlayer {
--     playerId :: PlayerId,
--   , numberOfDice :: NumberOfDice
-- } deriving (Show)
-- type PerudoResult = Int

instance Random Dice where
  randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of (x, g') -> (toEnum x, g')
  random = randomR (minBound, maxBound)
