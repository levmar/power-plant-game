-----------------------------------------------------------------------------
--
-- Module      :  GameEngineAPI
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

module GameEngineAPI (
  Game (Game),
  gamePlayers,
  gameBoard,
  gamePowerPlants,

  GameState,
  GameStep,

  completeAuction,
  endBuyPowerPlantsPhase,

  placeFuelOrder,
  endBuyRawMaterialsPhase,


) where

import BoardAPI
import PlayerAPI
import AuctionAPI
import PowerPlantAPI
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map


data Game = Game {
  gamePlayers :: [Player],
  gamePowerPlants :: [PowerPlant],
  gameBoard :: Board
}



data GameStep = Step1 | Step2 | Step3


data GameState = GameState {
  game :: Game,
  finalStep :: Bool,
  boardState :: BoardState,
  auctionablePowerPlants :: [PowerPlant],
  visiblePowerPlants :: [PowerPlant],
  hiddenPowerPlantStack :: [PowerPlant],
  playerStates :: [PlayerState],
  currentTurnOrder :: [Player],
  fuelForPurchase :: Map Fuel Int
}

class TurnPhaseState a where
  gameState :: a -> GameState

-- game state
-- players who still need to auction or pass
-- already-won auctions
data BuyPowerPlantsState = BuyPowerPlantsState GameState [Player] [CompletedAuction]

-- game state
-- players who still need to buy 0 or more materials
data BuyRawMaterialsState = BuyRawMaterialsState GameState [Player]

-- game state
-- players who still need to buy 0 or more cities
data BuildCitiesState = BuildCitiesState GameState [Player]

-- game state
-- players who still need to burn fuel at 0 or more plants
data BurnFuelState = BurnFuelState GameState [Player]

data EndOfTurn = CompletedGame GameState
               | NextTurn GameState


instance TurnPhaseState BuyPowerPlantsState where
  gameState (BuyPowerPlantsState gs _ _) = gs

instance TurnPhaseState BuyRawMaterialsState where
  gameState (BuyRawMaterialsState gs _) = gs

instance TurnPhaseState BuildCitiesState where
  gameState (BuildCitiesState gs _) = gs

instance TurnPhaseState BurnFuelState where
  gameState (BurnFuelState gs _) = gs

instance TurnPhaseState EndOfTurn where
  gameState (CompletedGame gs) = gs
  gameState (NextTurn gs) = gs


computeTurnOrder :: GameState -> Player -> Player -> Ordering
computeTurnOrder gs p1 p2 = let ps1 = findPlayerState p1 gs
                                ps2 = findPlayerState p2 gs
                                n1 = length $ getPlayerCities p1 gs
                                n2 = length $ getPlayerCities p2 gs
                             in compare n1 n2

findPlayerState :: Player -> GameState -> PlayerState
findPlayerState p gs = head $ filter (\ps -> (player ps) == p) (playerStates gs)

startTurn :: GameState -> BuyPowerPlantsState
startTurn gs = BuyPowerPlantsState gs' availableBidders []
               where newOrder = sortBy (computeTurnOrder gs) (gamePlayers (game gs))
                     availableBidders = newOrder
                     gs' = gs { currentTurnOrder = newOrder }

completeAuction :: BuyPowerPlantsState -> CompletedAuction -> BuyPowerPlantsState
completeAuction (BuyPowerPlantsState gs avail completed) a @ (CompletedAuction _ (PlayerBid p _)) =
    BuyPowerPlantsState gs (delete p avail) (completed ++ [a])


endBuyPowerPlantsPhase :: BuyPowerPlantsState -> BuyRawMaterialsState
endBuyPowerPlantsPhase (BuyPowerPlantsState gs players auctions) = BuyRawMaterialsState gs' (currentTurnOrder gs)
                                 where oldPlayerStates = (playerStates gs)
                                       gs' = gs {
                                         playerStates = transactAuctions oldPlayerStates auctions
                                       }

transactAuctions :: [PlayerState] -> [CompletedAuction] -> [PlayerState]
transactAuctions playerStates auctions = map (transactPlayerAuctions auctions) playerStates



transactPlayerAuctions :: [CompletedAuction] -> PlayerState -> PlayerState
transactPlayerAuctions auctions ps = let p = player ps
                                         relevantAuctions = filter (didPlayerWin p) auctions
                                         amountOwed = sum $ map winningBidAmount relevantAuctions
                                         newPlants = map completedAuctionPlant relevantAuctions
                                         newPlantStates = map initialPowerPlantState newPlants
                                       in ps {
                                         balance = (balance ps) - amountOwed,
                                         playerPendingPlantStates = (playerPendingPlantStates ps) ++ newPlantStates
                                       }


playerBalance :: Player -> GameState -> Int
playerBalance p gs = balance $ findPlayerState p gs

placeFuelOrder :: BuyRawMaterialsState -> FuelOrder -> Player -> Either String BuyRawMaterialsState
placeFuelOrder (BuyRawMaterialsState gs players) order p =
       let bal = playerBalance p gs
           cost = priceOfFuelOrder order (fuelForPurchase gs)
        in
          if (p /= head players)
            then Left $ "It is " ++ (show $ head players) ++ "'s turn"
            else if (bal < cost)
                 then  Left $ "Insufficient balance: " ++ (show cost) ++ " required, but only " ++ (show bal) ++ " available"
                 else
                   case validateFuelOrder order of
                     Left errs -> Left $ "Inconsistent fuel order: " ++ (unwords errs)
                     Right _ ->
                       Right $ BuyRawMaterialsState gs' players'
                                 where qtyMap = fuelForPurchase gs
                                       adjustPlayerState = (\ps -> if (p == (player ps)) then (applyFuelOrderToPlayer order qtyMap ps) else ps)
                                       gs' = gs {
                                         fuelForPurchase = applyFuelOrder order qtyMap,
                                         playerStates = map adjustPlayerState (playerStates gs)
                                       }
                                       players' = delete p players

--TODO
endBuyRawMaterialsPhase :: BuyRawMaterialsState -> BuildCitiesState
endBuyRawMaterialsPhase (BuyRawMaterialsState gs players) = BuildCitiesState gs (reverse $ currentTurnOrder gs)

--TODO
endBuildCitiesPhase :: BuildCitiesState -> BurnFuelState
endBuildCitiesPhase (BuildCitiesState gs players) = BurnFuelState gs (reverse $ currentTurnOrder gs)

--TODO
endTurn :: BurnFuelState -> Either EndOfTurn String
endTurn _ = Right "Game over"

gameStep :: GameState -> GameStep
gameStep gameState = if (finalStep gameState) then Step3
                     else if (maxOccupiedCities gameState) > 8 then Step2
                     else Step1


getGamePlayers :: GameState -> [Player]
getGamePlayers gameState = gamePlayers (game gameState)

isPlayerAtCity :: Player -> CityState -> Bool
isPlayerAtCity p c = (Just p) `elem` [player10 c, player15 c, player20 c]

getPlayerCities :: Player -> GameState -> [CityState]
getPlayerCities p gs = filter (isPlayerAtCity p) (cityStates (boardState gs))

maxOccupiedCities :: GameState -> Int
maxOccupiedCities gameState = maximum (map (\p -> length (getPlayerCities p gameState)) (gamePlayers (game gameState)))


initialFuelSupply :: Map Fuel Int
initialFuelSupply = Map.fromList [
    (Coal, 10),
    (Oil, 10),
    (Garbage, 6),
    (Nuclear, 2)
  ]


priceMapNuclear :: Map Int Int
priceMapNuclear = Map.fromList $ [(1,16),(2,14),(3,12),(4,10)] ++ [(x,y) | x <- [5,13], y <- [8,1], x + y == 13]

priceMapGarbage :: Map Int Int
priceMapGarbage = Map.fromList $ [(x,y) | x <- [1,24], y <- [8,1], x + y == 13]

priceMapOilOrCoal :: Map Int Int
priceMapOilOrCoal = Map.fromList $ [(x,y) | x <- [1,24], y <- [8,1], x + y == 13]

defaultQty :: Int
defaultQty = 0

unreachablePrice :: Int
unreachablePrice = 99999

priceLookup :: Map Int Int -> Fuel -> FuelSupply -> Int
priceLookup priceMap f qtyMap = Map.findWithDefault unreachablePrice (Map.findWithDefault defaultQty f qtyMap) priceMap

priceOf :: Fuel -> FuelSupply -> Int
priceOf Nuclear qtyMap = priceLookup priceMapNuclear   Nuclear qtyMap
priceOf Garbage qtyMap = priceLookup priceMapGarbage   Garbage qtyMap
priceOf Coal    qtyMap = priceLookup priceMapOilOrCoal Coal    qtyMap
priceOf Oil     qtyMap = priceLookup priceMapOilOrCoal Oil     qtyMap


removeFuel :: Fuel -> FuelSupply -> FuelSupply
removeFuel f map = removeFuelMany f 1 map

removeFuelMany :: Fuel -> Int -> FuelSupply -> FuelSupply
removeFuelMany f n map = Map.adjust (subtract n) f map

priceOfMany :: Fuel -> Int -> FuelSupply -> Int
priceOfMany f n map = if (n <= 0)
                        then 0
                        else (priceOf f map) + (priceOfMany f (n-1) (removeFuel f map))


type FuelStoragePlan = Map PowerPlant Int

-- number to order
-- where you intend to store them
data FuelOrderItem = FuelOrderItem Int FuelStoragePlan

type FuelOrder = Map Fuel FuelOrderItem
type FuelSupply = Map Fuel Int

type FuelOrderQty = Map Fuel Int

fuelOrderQuantities :: FuelOrder -> FuelOrderQty
fuelOrderQuantities order = Map.fromList $ map (\(fuel,FuelOrderItem qty _) -> (fuel,qty)) (Map.toList order)

priceOfFuelOrder :: FuelOrder -> FuelSupply -> Int
priceOfFuelOrder order qtyMap = sum $ map (\(k,v) -> priceOfMany k v qtyMap) (Map.toList $ fuelOrderQuantities order)


applyFuelOrder :: FuelOrder -> FuelSupply -> FuelSupply
applyFuelOrder order qtyMap = Map.mapWithKey (\k -> \v -> v - (Map.findWithDefault 0 k (fuelOrderQuantities order))) qtyMap

-- validate that a FuelOrder is internally consistent
-- i.e., that the storage plan accounts for the fuel being purchased, no more or less
validateFuelOrder :: FuelOrder -> Either [String] FuelOrder
validateFuelOrder order = if (length errs > 0) then Left errs else Right order
  where errs = Map.foldrWithKey checkItem [] order
        checkItem fuel (FuelOrderItem qty fuelStoragePlan) accum =
            if (qty /= (sum $ Map.elems fuelStoragePlan))
              then accum ++ ["Error: inconsistent fuel storage strategy"]
              else accum


quantityToStoreAtPowerPlant :: FuelOrder -> PowerPlant -> Int
quantityToStoreAtPowerPlant order pp =
    case Map.lookup (plantFuel pp) order of
      Just (FuelOrderItem _ fuelStoragePlan) ->
          Map.findWithDefault 0 pp fuelStoragePlan
      _ -> 0

applyFuelOrderToPowerPlantState :: FuelOrder -> PowerPlantState -> PowerPlantState
applyFuelOrderToPowerPlantState order ps =
    ps {
      availableFuel = (availableFuel ps) + (quantityToStoreAtPowerPlant order (powerPlant ps))
    }

applyFuelOrderToPlayer :: FuelOrder -> FuelSupply -> PlayerState -> PlayerState
applyFuelOrderToPlayer order qtyMap playerState =
        playerState {
          balance = (balance playerState) - (priceOfFuelOrder order qtyMap),
          playerActivePlantStates = map (applyFuelOrderToPowerPlantState order) (playerActivePlantStates playerState)
        }

initialGameState :: Game -> GameState
initialGameState g = let shuffledDeck = gamePowerPlants g
                     in
                       GameState {
                         game = g,
                         finalStep = False,
                         boardState = initialBoardState (gameBoard g),
                         auctionablePowerPlants = take 4 shuffledDeck,
                         visiblePowerPlants = take 4 (drop 4 shuffledDeck),
                         hiddenPowerPlantStack = drop 8 shuffledDeck,
                         playerStates = map initialPlayerState (gamePlayers g),
                         currentTurnOrder = (gamePlayers g),
                         fuelForPurchase = initialFuelSupply
                       }


-- determine turn order
-- auction power plants
-- buy raw materials
-- occupy cities
-- burn fuel
-- replenish fuel
-- (step 1 and 2) replace highest value plant
-- (step 3) replace lowest value plant



