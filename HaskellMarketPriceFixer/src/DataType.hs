{-
data Product = Product {name :: String, price :: Float, quantity :: Integer} deriving (Show)
data Buyer = Buyer {cash :: Float, info :: [([Char], Float)], totalUtil :: Float, purchased :: [Product]} deriving (Show)
data Seller = Seller {products :: [Product]} deriving (Show)
data Market = Market {buyers :: [Buyer], sellers :: [Seller]} | Empty deriving (Show)
-}

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
