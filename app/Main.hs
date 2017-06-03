module Main where

import Perudo.Game
import Perudo.Types
import Perudo.Bots.Trivial

players = [
    Player 0 expectedValueCaller 5 PalificoPending
  , Player 1 expectedValueCaller 5 PalificoPending
  , Player 2 expectedValueCaller 5 PalificoPending
  , Player 3 expectedValueCaller 5 PalificoPending]

-- main :: IO ()
main = simulateGame players
