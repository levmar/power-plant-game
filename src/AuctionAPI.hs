-----------------------------------------------------------------------------
--
-----------------------------------------------------------------------------

module AuctionAPI (

  AuctionState(AuctionState),
  auctionPlant,
  bidders,
  bestBid,

  Bid(PlayerBid),

  AuctionStateOrError,

  CompletedAuction(CompletedAuction),

  bid,
  pass,

  winningBid,
  minimumBid,

  startAuction,
  didPlayerWin,
  completedAuctionPlant,
  winningBidAmount

) where

import PlayerAPI
import PowerPlantAPI

data Bid = PlayerBid Player Int

instance Show Bid where
  show (PlayerBid p amt) = "Player " ++ (show p) ++ " bid " ++ (show amt)

data AuctionState = AuctionState {
  auctionPlant :: PowerPlant,
  bidders :: [Player],
  bestBid :: Maybe Bid
}

data CompletedAuction = CompletedAuction PowerPlant Bid

instance Show AuctionState where
  show a = "Auction for plant: ["
           ++ show (auctionPlant a)
           ++ "] with minimum bid "
           ++ show (minimumBid a)


minimumBid :: AuctionState -> Int
minimumBid a = case (bestBid a) of
                 Just (PlayerBid _ amt) -> amt + 1
                 _                      -> (plantInitialCost (auctionPlant a))

type AuctionStateOrError = Either String AuctionState


-- input: state of the bidding player, amount of the bid, and the state of the auction prior to the bid
-- output: either a modified auction state (representing a successful bid) or an error message
bid :: PlayerState -> Int -> AuctionState -> AuctionStateOrError
bid playerState amount a = let minBidAmount = minimumBid a
                            in
                              if (amount >= minBidAmount)
                                then Right a {
                                  bestBid = Just $ PlayerBid (player playerState) amount,
                                  bidders = (tail (bidders a)) ++ [head (bidders a)]
                                }
                                else Left $ "err: bid must be at least " ++ (show minBidAmount)

-- input: state of the bidding player, and the state of the auction prior to the bid
-- output: either a modified auction state (representing a successful bid) or an error message
pass :: PlayerState -> AuctionState -> AuctionStateOrError
pass playerState a = if ((player playerState) == head (bidders a))
                     then Right  $ a {bidders = tail (bidders a) }
                     else Left $ "err: it is " ++ (show $ head $ bidders a) ++ "'s turn"

-- if an auction represents a "completed" auction, return the winning bid, otherwise return Nothing
winningBid :: AuctionState -> Maybe Bid
winningBid a = if (1 == length (bidders a))
               then bestBid a
               else Nothing



-- inputs: the state of a player initiating an auction, a list of eligible bidders, and the plant to auction
-- outputs: either an auction or an error message
startAuction :: PlayerState -> [Player] -> PowerPlant -> AuctionStateOrError
startAuction playerState players powerPlant =
  let p = head players
      minBid = plantInitialCost powerPlant
  in
    bid playerState minBid AuctionState {
      auctionPlant = powerPlant,
      bidders = players,
      bestBid = Nothing
    }

didPlayerWin :: Player -> CompletedAuction -> Bool
didPlayerWin p (CompletedAuction _ (PlayerBid p2 _)) = p == p2


winningBidAmount :: CompletedAuction -> Int
winningBidAmount (CompletedAuction _ (PlayerBid p amount)) = amount

completedAuctionPlant :: CompletedAuction -> PowerPlant
completedAuctionPlant (CompletedAuction p _) = p
