{-----types and datatypes-----}
type Name = String
type Money = Float
type Price = Float
type Cost = Float
type Utility = Float
type Quantity = Int
data Inv {-short for inventory-} = I {pname :: Name, quantity :: Float, cost :: Float} deriving (Show) --money is the price paid, usually. You will need to decouple Sellers from prices, at least at first.
data Want = W { wname :: Name, utility :: Float, d :: Float } deriving (Show)
type PricingScheme = [(Name, Money)] -- store them separately, or create a different kind of seller for a seller with prices.

data MarketParticipant = Seller Name [Inv] Money | Buyer Name [Want] Money Utility deriving (Show)
data Offer = O { s :: Seller, b :: Buyer, sellerP :: Inv, buyerP :: Want, pTrade :: Float, qTrade :: Float, utilAndProfit :: Float } deriving (Show)

data Seller = S {sname :: Name, inv :: [Inv], profit :: Float} deriving (Show)
data Buyer = B {bname :: Name, wants :: [Want], budget :: Float, bUtil :: Utility} deriving (Show)
data Market = M { buyers::[Buyer], sellers::[Seller], ps::PricingScheme } deriving (Show)

{-----helper functions-----} 
buyers_Interested buyers pName = filter(\x-> pName `elem` (map (wname) $ wants x)) buyers
sellers_WithInv sellers pName = filter(\x-> pName `elem` (map (pname) $ inv x)) sellers
supplyList (sellers) = foldl (\ret x-> if (pname x) `elem` ret then ret else (pname x):ret) ret invs
  where invs = concat $ map (inv) sellers
        ret = []

getOffers seller buyer pName = [ O seller buyer sellerP buyerP pTrade (getQTrade pTrade) ((totalUtil pTrade)+pTrade*(getQTrade pTrade)) | pTrade <- [0.0..c*2.0] ]
  where sellerP = head $ filter (\i-> pname i == pName) $ inv seller
        buyerP = head $ filter (\w -> wname w == pName) $ wants buyer
        c = cost sellerP
        getQTrade p = 1.0 * min ((budget buyer)/ p) (quantity sellerP)
        totalUtil p = sum $ [ u | u <- [utility buyerP, (utility buyerP)-(d buyerP)..(utility buyerP) - (d buyerP * (getQTrade p))] ]
        profit p q = q*p

{-----main operations-----}
{-
transaction (M [] _ ps) = ps
transaction (M _ [] ps) = ps
transaction input@(M (b:buyers) sellers emptyps) output@(M bs ss ps) = transaction (M buyers sellers emptyps) (M (b:bs) sellers (bestOffer:ps))
  where activeSeller b = 
        bestOffer = filter () (getOffers activeSeller b 
-}
{------test-----}
want1 = W "laptop" 1000 500 
want2 = W "tea" 20 10
inv1 = I "laptop" 1 300
inv2 = I "noodles" 30 5
buyer1 = B "karen" [want1, want2] 100 0
buyer2 = B "irene" [want1] 100 0
seller1 = S "geoff" [inv1] 0
seller2 = S "vivian" [inv2] 0

data T1 = AA { num1 :: Float} deriving (Show)
data T2 = BB { num2 :: Float } deriving (Show)
z1 = AA 30
z2 = BB 100.4
z3 = [ AA (f n) | n <- [1..10]]
  where f n= n+1
