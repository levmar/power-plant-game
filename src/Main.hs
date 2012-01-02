-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main (
    main
) where

import GameEngineAPI
import GameEngineAuctionAPI
import PlayerAPI
import PowerPlantAPI
import BoardAPI

import Control.Monad
import Control.Monad.Instances

import Data.Maybe (listToMaybe)


players = [p1,p2,p3]
playerStates1 = map initialPlayerState players


sampleBoard = makeBoard [
  (PowerLine (City "a") (City "b") 5),
  (PowerLine (City "b") (City "c") 3),
  (PowerLine (City "b") (City "d") 7),
  (PowerLine (City "a") (City "d") 4)
 ]


sampleGame = Game {
  gamePlayers = players,
  gameBoard = sampleBoard,
  gamePowerPlants = [
    PowerPlant Coal 3 1 1,
    PowerPlant Coal 4 2 2,
    PowerPlant Oil 5 2 2,
    PowerPlant Coal 6 2 3,
    PowerPlant Coal 7 3 3,
    PowerPlant Garbage 8 1 2
  ]
}



main = runGame

runGame = do
          let ac = (startAuction (head playerStates1) players sampleCoalPlant)
           in
             case ac of
               Left auction ->
                     do
                       a' <- runAuction auction playerStates1
                       case (winningBid a') of
                         Nothing ->
                           putStrLn "No winning bid"
                         Just (PlayerBid p amt) ->
                           putStrLn $ "Won by " ++ (show p) ++ " for " ++ (show amt)
               Right err ->
                     putStrLn $ "Unable to start auction: " ++ err



runAuction :: AuctionState -> [PlayerState] -> IO AuctionState
runAuction a ps = case (winningBid a) of
                    Nothing ->
                      do
                        putStrLn $ "There are " ++ (show $ bidders a) ++ " bidders\n"
                        putStrLn $ "Time to bid, " ++ (show $ head $ bidders a) ++ "\n"
                        putStrLn $ (show a) ++ "\n"
                        bid <- getLine
                        if ("pass" == bid) then
                            handlePass a ps
                          else
                            handleBid a ps bid
                    Just x ->
                      do
                        putStrLn "Auction complete\n"
                        return a

findStateForPlayer :: [PlayerState] -> Player -> Maybe PlayerState
findStateForPlayer ps p = listToMaybe $ filter (\state -> (player state) == p) ps


handlePass :: AuctionState -> [PlayerState] -> IO AuctionState
handlePass a ps = let
                    mplayerState = findStateForPlayer ps (head $ bidders a)
                    passed = case mplayerState of
                               Just playerState -> pass playerState a
                               _ -> Right "invalid player"
                  in
                    case passed of
                      Right err -> do
                                     putStrLn $ "ERROR: " ++ err
                                     runAuction a ps

                      Left a'   -> do
                                     putStrLn $ "OKAY"
                                     runAuction a' ps


handleBid :: AuctionState -> [PlayerState] -> String -> IO AuctionState
handleBid a ps v = let
                    bidAmt = (read v :: Int)
                    mplayerState = findStateForPlayer ps (head $ bidders a)
                    result = case mplayerState of
                               Just playerState -> bid playerState bidAmt a
                               _ -> Right "invalid player"
                   in
                    case result of
                      Right err -> do
                                     putStrLn $ "ERROR: " ++ err
                                     runAuction a ps
                      Left a'   -> do
                                     runAuction a' ps

p1 :: Player
p1 = MakePlayer {
  playerName = "Player 1"
}

p2 = MakePlayer {
  playerName = "Player 2"
}

p3 = MakePlayer {
  playerName = "Player 3"
}

sampleCoalPlant = PowerPlant {
                  plantFuel = Coal,
                  plantInitialCost = 6,
                  plantRequires = 3,
                  plantSupplies = 7
                }

sampleAuctionState1 = case startAuction (initialPlayerState p1) [p1, p2] sampleCoalPlant of
                        Left x -> x




-- expect acution state with p2 as the only eligible bidder, no min bid
sampleAuctionState2 = pass (initialPlayerState p1) sampleAuctionState1

-- expect error (not your turn)
sampleAuctionState2b = case sampleAuctionState2 of
                         Left a -> pass (initialPlayerState p1) a
                         _ -> Right "err: starting state invalid"


-- expect error (bid too low)
sampleAuctionState3 = bid (initialPlayerState p1) 3 sampleAuctionState1


-- expect auction state with p2 as next bidder, p1 as best bidder
sampleAuctionState4 = bid (initialPlayerState p1) 6 sampleAuctionState1

sampleBoardState = initialBoardState sampleBoard
