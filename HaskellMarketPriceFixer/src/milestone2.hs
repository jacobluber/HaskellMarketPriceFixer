type Name = String
type Money = Float
type Price = Float
type Utility = Float
type Quantity = Int
data Inv {-short for inventory-} = I {pname :: Name, q :: Quantity, p :: Price} --money is the price paid, usually. You will need to decouple Sellers from prices, at least at first.
data Want = W { wname :: Name, initUtil :: Utility, d :: Utility } deriving (Show)
type PricingScheme = [(Name, Money)] -- store them separately, or create a different kind of seller for a seller with prices.

data MarketParticipant = Seller Name [Inv] Money | Buyer Name [Want] Money Utility
data Offer = O { s :: Seller, b :: Buyer}

data Seller = S {sname :: Name, inv :: [Inv], profit :: Money}
data Buyer = B {bname :: Name, wants :: [Want], bUtil :: Utility} deriving (Show)
type Market = ([Buyer], [Seller], PricingScheme)

{- helper functions -} 
buyerInterested buyers pName = filter(\x-> pName `elem` (map (wname) $ wants x)) buyers

want1 = W "laptop" 1000 500 
want2 = W "tea" 20 10
buyer1 = B "karen" [want1, want2] 0
buyer2 = B "irene" [want1] 0
