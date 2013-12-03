module DataType where

data MarketParticipant = Seller {sellerId :: String, sellerGoodInfo :: [(String, Float, Float)], sellerMoney :: Float} | Buyer { buyerId :: String, buyerGoodInfo :: [(String, Float, Float, Float)], moneyHeld :: Float, accumulatedUtility :: Float} deriving (Show, Ord, Eq)
data EndMarket = EndParticipants {totalSellerProfit :: Float, totalBuyerUtility :: Float, participants :: [MarketParticipant]} deriving (Show, Ord, Eq)
data Market = Participants {marketParticipants :: [MarketParticipant]} | Empty deriving (Show, Ord, Eq)
data Offer = OfferInfo {sellerInfo :: String, goodInfo :: (String, Float, Float, Float)} | NoSeller deriving (Show, Ord, Eq)

testsellers = [Seller "A" [("a",4,1),("b",7,2),("c",5,3)] 0,Seller "B" [("a",1,3),("b",8,2),("c",2,1)] 0,Seller "C" [("a",0,2),("b",4,2),("c",1,2)] 0] 
testbuyers = [Buyer "Z" [("a",0,5,2),("b",0,7,1),("c",0,6,1)] 100 0, Buyer "Y" [("a",0,5,4),("b",0,4,2),("c",0,2,1)] 50 0,Buyer "X" [("a",0,8,3),("b",0,10,3),("c",0,8,7)] 250 0] 

--Offer SellerStringIdentifier (GoodIdentifier,UnitsOfGoodHeld,PriceChargedForGood,UtilityPerDollarForActiveBuyer) 
--Seller SellerStringIdentifier [(GoodIdentifier,UnitsOfGoodHeld,PricePaidForGood)] MoneyHeld
--Buyer BuyerStringIdentifier [(GoodIdentifier,UnitsOfGoodHeld,Utility,UtilityDecrementer)] MoneyHeld AccumulatedUtility 
--EndMarket TotalSellerProfit TotalBuyerUtility [MarketParticipant]

{-
data Product = Product {name :: String, price :: Float, quantity :: Integer} deriving (Show)
data Buyer = Buyer {cash :: Float, info :: [([Char], Float)], totalUtil :: Float, purchased :: [Product]} deriving (Show)
data Seller = Seller {products :: [Product]} deriving (Show)
data Market = Market {buyers :: [Buyer], sellers :: [Seller]} | Empty deriving (Show)
-}

{-
type Name = String
type Money = Float
type Price = Float
type PName = String
type Utility = Float
type Quantity = Int
type Inv {-short for inventory-} = (PName, Quantity, Price) --money is the price paid, usually. You will need to decouple Sellers from prices, at least at first.
type Want = (PName, Utility)
type PricingScheme = [(PName, Money)] -- store them separately, or create a different kind of seller for a seller with prices.
data MarketParticipant = Seller Name [Inv] Money | Buyer Name [Want] Money Utility

data Seller = S {sname :: Name, inventory :: [Inv], profit :: Money}
data Buyer = B {bname :: Name, wants :: [Want], utility :: Utility} deriving (Show)
type Market = ([Buyer], [Seller], PricingScheme)
-}