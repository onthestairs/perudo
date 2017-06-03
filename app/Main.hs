module Main where

import Data.List
import Control.Monad

import Perudo.Game
import Perudo.Types
import Perudo.Bots.Trivial

players = [
    Player 0 expectedValueCaller 5 PalificoPending
  , Player 1 expectedValueCaller 5 PalificoPending
  , Player 2 expectedValueCaller 5 PalificoPending
  , Player 3 expectedValueCaller 5 PalificoPending]

-- main :: IO ()
-- main = simulateGame players


main = do
  winners <- forM [1..1000] (const (simulateGame players))
  let winCounts = map length $ group $ sort winners
  return winCounts
