module Main where

import Data.List
import Control.Monad

import Perudo.Game
import Perudo.Types
import Perudo.Bots.Trivial

players = [
    Player 0 expectedValueCaller 5 PalificoPending
  , Player 1 expectedValueCaller 5 PalificoPending
  , Player 2 strategySixSixes 5 PalificoPending
  , Player 3 expectedValueCaller 5 PalificoPending]

-- main :: IO ()
-- main = simulateGame players


zipMap f xs = zip xs (map f xs)

summariseWins :: [PlayerId] -> [Player] -> [(PlayerId, Int)]
summariseWins winners players =
  let playerIds = map playerId players
  in zipMap (\playerId -> length $ filter (== playerId) winners) playerIds

main = do
  winners <- forM [1..1000] (const (simulateGame players))
  let summary = (summariseWins winners players)
  print summary
  return ()
