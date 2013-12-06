module DataType where
type Name = String
type Good = String
type Money = Float
type Utility = Float
type Stock = (Good, Int, Money) 
type Want = (Good, Int, Utility, Utility) 
type OfferGood = (String, Int, Money, Utility)

--data Seller = Seller  {sellerId :: String, stock:: [Stock], profit:: Money} deriving (Show, Eq)
--data Buyer = Buyer {buyerId :: String, want  :: [Want],  budget:: Money, totalUtility:: Utility}
--data Offer = Offer {seller :: String, good :: Good, quantity :: Int, price :: Money, utilityRatio :: Float}

data MarketParticipant = Seller {
        sellerId :: Name, sellerGoodInfo :: [Stock], sellerMoney :: Money} 
        | Buyer { buyerId :: Name, buyerGoodInfo :: [Want], moneyHeld :: Money, accumulatedUtility :: Utility} 
        deriving (Show, Ord, Eq)
data EndMarket = EndParticipants {
        totalSellerProfit :: Money, totalBuyerUtility :: Utility, participants :: [MarketParticipant]} 
        deriving (Show, Ord, Eq)
data Market = Participants {marketParticipants :: [MarketParticipant]} | Empty deriving (Show, Ord, Eq)
data Offer = OfferInfo {sellerInfo :: String, goodInfo :: OfferGood} | NoSeller deriving (Show, Ord, Eq)

testsellers = [Seller "A" [("a",4,1),("b",7,2),("c",5,3)] 0,
        Seller "B" [("a",1,3),("b",8,2),("c",2,1)] 0,
        Seller "C" [("a",0,2),("b",4,2),("c",1,2)] 0] 
testbuyers = [Buyer "Z" [("a",0,5,2),("b",0,7,1),("c",0,6,1)] 100 0, 
        Buyer "Y" [("a",0,5,4),("b",0,4,2),("c",0,2,1)] 50 0,
        Buyer "X" [("a",0,8,3),("b",0,10,3),("c",0,8,7)] 250 0] 

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
type Inv {-short for inventory-} = (PName, Quantity, Price) 
type Want = (PName, Utility)
type PricingScheme = [(PName, Money)] 
data MarketParticipant = Seller Name [Inv] Money | Buyer Name [Want] Money Utility

data Seller = S {sname :: Name, inventory :: [Inv], profit :: Money}
data Buyer = B {bname :: Name, wants :: [Want], utility :: Utility} deriving (Show)
type Market = ([Buyer], [Seller], PricingScheme)
-}