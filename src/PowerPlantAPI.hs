-----------------------------------------------------------------------------
--
-- Module      :  PowerPlantAPI
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

module PowerPlantAPI (
  Fuel(Coal,Oil,Garbage,Nuclear),
  PowerPlant (PowerPlant),
  plantFuel,
  plantInitialCost,
  plantRequires,
  plantSupplies,

  PowerPlantState (PowerPlantState),
  powerPlant,
  availableFuel,

  initialPowerPlantState

) where




data Fuel = Coal
          | Oil
          | Garbage
          | Nuclear
          deriving (Eq,Ord,Show)

data PowerPlant = PowerPlant {
  plantFuel :: Fuel,
  plantInitialCost :: Int,
  plantRequires :: Int,
  plantSupplies :: Int
}

instance Eq PowerPlant where
  (==) p1 p2 = (plantInitialCost p1) == (plantInitialCost p2)

instance Ord PowerPlant where
 compare p1 p2 = compare (plantInitialCost p1) (plantInitialCost p2)

instance Show PowerPlant where
  show p = show (plantInitialCost p)
            ++ ": "
            ++ show (plantRequires p)
            ++ " "
            ++ show (plantFuel p)
            ++ " to power "
            ++ show (plantSupplies p)
            ++ " cities"


data PowerPlantState = PowerPlantState {
  powerPlant :: PowerPlant,
  availableFuel :: Int
}

initialPowerPlantState :: PowerPlant -> PowerPlantState
initialPowerPlantState pp = PowerPlantState pp 0

