module Perudo.Game (
    simulateRound
  , simulateGame
) where

import System.Random
import Data.List.Split
import System.Random.Shuffle
import Control.Monad.Loops

import Perudo.Types

makeHands :: RandomGen g => [Int] -> g -> [Hand]
makeHands ns g = splitPlaces ns (randoms g)

wasCallCorrect :: DiceBet -> [Dice] -> Bool
wasCallCorrect (DiceBet n d) dices =
  length (filter (\dice -> (dice == One) || (dice == d)) dices) < n

playPerudo :: [InRoundPlayer] -> (PlayerId, [Action])
playPerudo players =
  let numberOfPlayers = length players
      playersNumberOfDice = map (numberOfDice . player) players
      actions = map (\(irp, turn) -> (strategy $ player irp) (hand irp) playersNumberOfDice (roundPosition irp) (take turn actions))
                    (zip (cycle players) [0..])
      actionPairs = zip actions (tail actions)
      activeBetPairs = takeWhile ((/= Call) . snd) actionPairs
      Bet finalBet = fst $ head $ dropWhile ((/= Call) . snd) actionPairs
      allDices = concatMap hand players
      callWasCorrect = wasCallCorrect finalBet allDices
      callingPlayerIndex = (length activeBetPairs) + 1
      callingPlayer = (cycle players) !! callingPlayerIndex
      lastBettingPlayer = (cycle players) !! (callingPlayerIndex - 1)
      loser = if callWasCorrect then lastBettingPlayer else callingPlayer
  in ((playerId . player) loser, (map fst activeBetPairs))

decrementPlayer :: PlayerId -> [Player] -> [Player]
decrementPlayer losingPlayerId players =
  let f player@(Player _ _ losingPlayerDice _) = player {numberOfDice = losingPlayerDice - 1}
      p player = playerId player == losingPlayerId
  in map (\player -> if p player then f player else player) players

rotatePlayers :: PlayerId -> [Player] -> [Player]
rotatePlayers losingPlayerId players =
  take (length players) $ dropWhile ((== 0) . numberOfDice) $ dropWhile ((/= losingPlayerId) . playerId) (cycle players)

simulateRound :: [Player] -> IO [Player]
simulateRound players = do
  g <- newStdGen
  let playersToPlay = filter ((> 0) . numberOfDice) players
  let n = length playersToPlay
  let hands = makeHands (map numberOfDice playersToPlay) g
  let inRoundPlayers = map (\(h, p, i) -> InRoundPlayer p h i) (zip3 hands playersToPlay [0..])
  let (losingPlayerId, actions) = playPerudo inRoundPlayers
  let newPlayers = decrementPlayer losingPlayerId players
  let newPlayersOrdered = rotatePlayers losingPlayerId newPlayers
  -- print actions
  -- print losingPlayerId
  -- print newPlayersOrdered
  return newPlayersOrdered

gameHasEnded ps = length (filter ((> 0) . numberOfDice) ps) == 1

simulateGame :: [Player] -> IO PlayerId
simulateGame players = do
  g <- newStdGen
  let shuffledPlayers = shuffle' players (length players) g
  endPlayers <- iterateUntilM gameHasEnded simulateRound shuffledPlayers
  let winningPlayer = playerId $ head $ filter ((> 0) . numberOfDice) endPlayers
  return winningPlayer
