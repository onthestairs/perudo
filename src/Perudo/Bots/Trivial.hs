module Perudo.Bots.Trivial (
    strategyCall
  , strategySixSixes
  , expectedValueCaller
) where

import Perudo.Types

strategyCall :: Hand -> [NumberOfDice] -> RoundPosition -> [Action] -> Action
strategyCall _ _ _ _ = Call

strategySixSixes :: Hand -> [NumberOfDice] -> RoundPosition -> [Action] -> Action
strategySixSixes _ ns _ as =
  let desiredBet = DiceBet (sum ns `div` 2) Six
  in case length as of
    0 -> Bet desiredBet
    otherwise ->
      let Bet b = last as
      in if b > desiredBet then Call else Bet desiredBet

expectedNumber hand ns dice =
  let handValues = length $ filter (\d -> d == One || d == Six) hand
      expectedInOtherHands = (sum ns) `div` 3
  in handValues + expectedInOtherHands

expectedValueCaller :: Hand -> [NumberOfDice] -> RoundPosition -> [Action] -> Action
expectedValueCaller _ _ _ [] = Bet (DiceBet 1 Two)
expectedValueCaller hand ns _ as =
  let Bet b@(DiceBet n d) = last as
      expectedN = expectedNumber hand ns d
  in if expectedN < n then Call else Bet (succ b)
