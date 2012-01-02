-----------------------------------------------------------------------------
--
-- Module      :  BoardAPI
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

module BoardAPI (
  City (City),
  player10,
  player15,
  player20,

  CityState,
  PowerLine (PowerLine),
  city1,
  city2,

  Board(Board),
  board,
  powerLines,
  cities,
  cityStates,
  BoardState(BoardState),

  makeBoard,
  initialCityState,
  initialBoardState
) where


import Data.List

import PlayerAPI

data City = City {
  cityName :: String
} deriving (Eq, Show)


data CityState = CityState{
  city :: City,
  player10 :: Maybe Player,
  player15 :: Maybe Player,
  player20 :: Maybe Player
}

data PowerLine = PowerLine {
  city1 :: City,
  city2 :: City,
  cost :: Int
}

instance Show PowerLine where
  show pl = "connects "
            ++ show (city1 pl)
            ++ " and "
            ++ show (city2 pl)
            ++ " at a cost of "
            ++ show (cost pl)


data Board = Board {
  powerLines :: [PowerLine],
  cities :: [City]
}

data BoardState = BoardState {
  board :: Board,
  cityStates :: [CityState]
}


makeBoard :: [PowerLine] -> Board
makeBoard lines = Board {
  powerLines = lines,
  cities = citiesForLines lines
}

-- get a unique list of cities on either end of the given power lines
citiesForLines :: [PowerLine] -> [City]
citiesForLines lines = nub $ (map (\l -> city1 l) lines) ++ (map (\l -> city1 l) lines)


initialCityState :: City -> CityState
initialCityState c = CityState {
  city = c,
  player10 = Nothing,
  player15 = Nothing,
  player20 = Nothing
}

initialBoardState :: Board -> BoardState
initialBoardState b = BoardState {
  board = b,
  cityStates = map initialCityState (cities b)
}

