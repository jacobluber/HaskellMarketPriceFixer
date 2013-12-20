module DataTypes where

-----------------------------DATATYPES-----------------------------

type Name = String
type Good = String
type Money = Float
type Utility = Float
type Unit = Int
type PreStock = (Good, Unit, Money)
type Stock = (Good, Unit, Money, Money)  --Money here is cost.
type Want = (Good, Unit, Utility, Utility) 
type OfferGood = (Good, Unit, Money, Utility) 
data Vendor = Vendor Name [Stock]
data MarketParticipant = 
        Seller { sellerId :: Name, sellerGoods :: [Stock], sellerProfit :: Money } 
        | Buyer { buyerId :: Name, buyerGoods :: [Want], moneyHeld :: Money, accUtil :: Utility } 
        deriving (Show, Ord, Eq)
data EndMarket = EndParticipants {
        totalSellerProfit :: Money, totalBuyerUtil :: Utility, participants :: [MarketParticipant]} 
        deriving (Show, Ord, Eq)
data Market = Participants {marketParticipants :: [MarketParticipant]} | Empty deriving (Show, Ord, Eq)
data Offer = OfferInfo {sellerInfo :: Name, goodInfo :: OfferGood} | NoOffer deriving (Show, Ord, Eq)

{-
Offer SellerStringIdentifier (GoodIdentifier,UnitsOfGoodHeld,PriceChargedForGood,UtilityPerDollarForActiveBuyer) 
Seller SellerStringIdentifier [(GoodIdentifier,UnitsOfGoodHeld,PricePaidForGood)] MoneyHeld
Buyer BuyerStringIdentifier [(GoodIdentifier,UnitsOfGoodHeld,Utility,UtilityDecrementer)] MoneyHeld accUtil 
EndMarket TotalSellerProfit TotalBuyerUtility [MarketParticipant]

data Seller = Seller  {sellerId :: String, stock:: [Stock], profit:: Money} deriving (Show, Eq)
data Buyer = Buyer {buyerId :: String, want  :: [Want],  budget:: Money, totalUtil:: Utility}
data Offer = Offer {seller :: String, good :: Good, quantity :: Int, price :: Money, utilityRatio :: Float}
-}