module Perudo.Bots.Trivial (
    strategyCall
  , strategySixSixes
) where

import Perudo.Types

strategyCall :: Hand -> RoundPosition -> [Action] -> Action
strategyCall _ _ _ = Call

strategySixSixes :: Hand -> RoundPosition -> [Action] -> Action
strategySixSixes _ _ _ = Bet (6, Six)
