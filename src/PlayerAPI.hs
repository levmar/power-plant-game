-----------------------------------------------------------------------------
--
-- Module      :  PlayerAPI
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module PlayerAPI (
  Player (MakePlayer),
  playerName,

  PlayerState,
  balance,
  initialPlayerState,
  playerActivePlantStates,
  playerPendingPlantStates,
  player


) where

import PowerPlantAPI

data Player = MakePlayer {
  playerName :: String
} deriving (Eq,Show)

data PlayerState = MakePlayerState {
  player :: Player,
  playerActivePlantStates :: [PowerPlantState],
  playerPendingPlantStates :: [PowerPlantState],
  balance :: Int
}



initialPlayerState :: Player -> PlayerState
initialPlayerState p = MakePlayerState {
  player = p,
  playerActivePlantStates = [],
  playerPendingPlantStates = [],
  balance = 50
}
