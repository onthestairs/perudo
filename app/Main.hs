module Main where

import Perudo.Game
import Perudo.Bots.Trival

players = [
    Player strategySixSixes 5 PalificoPending
  , Player strategyCall 5 PalificoPending]

-- main :: IO ()
main = simulateRound players
