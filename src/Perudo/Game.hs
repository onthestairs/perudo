module Perudo.Game (
  simulateRound
) where

import System.Random
import Data.List.Split
import Debug.Trace (trace)

import Perudo.Types

makeHands :: RandomGen g => [Int] -> g -> [Hand]
makeHands ns g = splitPlaces ns (randoms g)

callWasCorrect :: DiceBet -> [Dice] -> Bool
callWasCorrect (n, d) dices =
  length (filter (\dice -> (dice == One) || (dice == d)) dices) < n

playPerudo :: [InRoundPlayer] -> PerudoResult
playPerudo players =
  let numberOfPlayers = length players
      actions = map (\(irp, turn) -> (strategy $ player irp) (hand irp) (roundPosition irp) (take turn actions))
                    (zip (cycle players) [0..])
      actionPairs = zip actions (tail actions)
      activeBetPairs = takeWhile ((/= Call) . snd) actionPairs
      finalBet = fst $ head $ dropWhile ((/= Call) . snd) actionPairs
      -- finalBet = (fst . last) activeBetPairs
      allDices = concatMap hand players
      callWasCorrect = True
      callingPlayer = length activeBetPairs `mod` numberOfPlayers
      lastBettingPlayer = (callingPlayer + (numberOfPlayers - 1))  `mod` numberOfPlayers
      loser = if callWasCorrect then lastBettingPlayer else callingPlayer
  in trace ("Final bet: " ++ show finalBet) loser

changeNthElement :: Int -> a -> [a] -> [a]
changeNthElement idx x list
    | idx < 0   = list
    | otherwise = case splitAt idx list of
                    (front, _:back) -> front ++ x : back
                    _ -> list    -- if the list doesn't have an element at index idx

decrementPlayer :: Int -> [Player] -> [Player]
decrementPlayer losingPlayerIndex players =
  let losingPlayer = players !! losingPlayerIndex
      losingPlayerDice = numberOfDice losingPlayer
      newLosingPlayer = losingPlayer {numberOfDice = losingPlayerDice - 1}
  in changeNthElement losingPlayerIndex newLosingPlayer players

simulateRound :: [Player] -> IO [Player]
simulateRound players = do
  g <- newStdGen
  let n = length players
  let hands = makeHands (map numberOfDice players) g
  let inRoundPlayers = map (\(h, p, i) -> InRoundPlayer p h i) (zip3 hands players [0..])
  let losingPlayerIndex = playPerudo inRoundPlayers
  let newPlayers = decrementPlayer losingPlayerIndex players
  return newPlayers
