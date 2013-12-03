module OldWork where

{-


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




module PFixerMultGoods where

data MarketParticipant = Seller String [(String, Float, Float)] Float | Buyer String [(String, Float, Float)] Float deriving (Show, Ord, Eq)
data Market = Participants [MarketParticipant] | Empty deriving (Show, Ord, Eq)
data SpecialSeller = SellerU String (String, Float, Float, Float) | NoSeller deriving (Show, Ord, Eq)
testsellers = [Seller "A" [("a",4,43),("b",7,34),("c",5,23)] 0,Seller "B" [("a",1,34),("b",8,12),("c",2,34)] 0,Seller "C" [("a",0,2),("b",4,17),("c",1,27)] 0] 
testbuyers = [Buyer "Z" [("a",0,50),("b",0,25),("c",0,10)] 100, Buyer "Y" [("a",0,10),("b",0,50),("c",0,25)] 50,Buyer "X" [("a",0,25),("b",0,10),("c",0,50)] 250] 
--SpecialSeller SellerStringIdentifier (GoodIdentifier,UnitsOfGoodHeld,PriceChargedForGood,UtilityPerDollarForActiveBuyer) 
--Seller SellerStringIdentifier [(GoodIdentifier,UnitsOfGoodHeld,PriceChargedForGood)] MoneyHeld
--Buyer BuyerStringIdentifier [(GoodIdentifier,UnitsOfGoodHeld,UtilityGainedFromHaving1UnitOfGood)] MoneyHeld

fTup (x,_,_,_) = x
sTup (_,x,_,_) = x
tTup (_,_,x,_) = x
frTup (_,_,_,x) = x

matchMarketParticipants :: [SpecialSeller] -> [SpecialSeller] -> MarketParticipant -> (MarketParticipant,SpecialSeller,SpecialSeller,Float)
matchMarketParticipants sellersWithInventory dudSellers activeBuyer = if sellersWithInventory == [] 
                                                                      then (activeBuyer, NoSeller, NoSeller, 0) 
                                                                      else let utilityPerSeller = map (\(SellerU a (b,c,d,e)) -> e) sellersWithInventory
                                                                               mostUtility = maximum utilityPerSeller
                                                                               activeSeller = head [SellerU a (b,c,d,e)|SellerU a (b,c,d,e) <- sellersWithInventory,e == mostUtility]
                                                                               tempInactiveSellers = [SellerU a (b,c,d,e)|SellerU a (b,c,d,e) <- sellersWithInventory,e /= mostUtility]
                                                                               buyerMoneyHeld = (\(Buyer x y z) -> z) activeBuyer
                                                                               sellerPrice = (\(SellerU a (b,c,d,e)) -> d) activeSeller
                                                                               sellerQuantity = (\(SellerU a (b,c,d,e)) -> c) activeSeller
                                                                               sellerUtility = (\(SellerU a (b,c,d,e)) -> e) activeSeller
                                                                               sellerGood = (\(SellerU a (b,c,d,e)) -> b) activeSeller
                                                                               sellerIdent =  (\(SellerU a (b,c,d,e)) -> a) activeSeller
                                                                            in if buyerMoneyHeld > sellerPrice then let relevantBuyerGoods = (\(Buyer a b c) -> b) activeBuyer
                                                                                                                        goodIdent = (\(SellerU a (b,c,d,e)) -> b) activeSeller
                                                                                                                        relevantRelevantBuyerGood = head [(a,b,c)|(a,b,c) <-relevantBuyerGoods,a == goodIdent]
                                                                                                                        irrelevantRelevantBuyerGoods = [(a,b,c)|(a,b,c) <- relevantBuyerGoods,a /= goodIdent]
                                                                                                                        newGood = (\(a,b,c) -> (a,b+1,c)) relevantRelevantBuyerGood
                                                                                                                        newGoods = [newGood] ++ irrelevantRelevantBuyerGoods 
                                                                                                                        oldBuyerMoney = (\(Buyer a b c) -> c) activeBuyer 
                                                                                                                        buyerIdent = (\(Buyer a b c) -> a) activeBuyer 
                                                                                                                        updateBuyer = (Buyer buyerIdent newGoods (oldBuyerMoney-sellerPrice))                                                                                                                                                                                                                                                                                                            
                                                                                                                        updateActiveSeller = SellerU sellerIdent (sellerGood, sellerQuantity-1, sellerPrice, sellerUtility)                                                                                                                        
                                                                                                                    in (updateBuyer, updateActiveSeller, activeSeller, sellerPrice)                                                                                                                   
                                                                                                                    else matchMarketParticipants tempInactiveSellers ([activeSeller] ++ dudSellers) activeBuyer                                                                                                                                                                                 

priceFix:: [MarketParticipant] -> [MarketParticipant] -> Market -> Market
priceFix buyers sellers result = if buyers == [] 
                                 then let unpackedResult = (\(Participants x) -> x) result
                                          in Participants (unpackedResult++sellers)
                                 else let (Buyer identifier goods money) = head(buyers)
                                          inactiveBuyers = tail(buyers)
                                          buyerUtility = map (\(a,b,c) -> c) goods 
                                          sellerVals = map (\(Seller a b c) -> map (\b -> (a,b)) b) sellers
                                          specialSellerInput = concat (map (\t -> zip buyerUtility t) sellerVals)
                                          generateSpecialSellers = map (\(a, (b, (c, d, e))) -> SellerU b (c,d,e,a/e)) specialSellerInput
                                          specialSellersWithInventory = [SellerU a (b,c,d,e)| SellerU a (b,c,d,e) <- generateSpecialSellers, c>0]
                                          specialSellersWithoutInventory = [SellerU a (b,c,d,e)| SellerU a (b,c,d,e) <- generateSpecialSellers, c==0]
                                          transaction = matchMarketParticipants specialSellersWithInventory [] (Buyer identifier goods money)
                                          updatedBuyer = fTup(transaction)
                                          updatedSeller = sTup(transaction)
                                          originalSeller = tTup(transaction)
                                          moneyToSeller = frTup(transaction)
                                          unpackedInitialResult = (\(Participants x) -> x) result
                                      in if (updatedBuyer == (Buyer identifier goods money)) && (result == Empty) 
                                         then priceFix inactiveBuyers sellers (Participants ([(Buyer identifier goods money)]))
                                         else if updatedBuyer == (Buyer identifier goods money)
                                              then priceFix inactiveBuyers sellers  (Participants ([(Buyer identifier goods money)]++unpackedInitialResult))
                                              else let newSellerIdent =  (\(SellerU a (b,c,d,e)) -> a) originalSeller
                                                       relevantSeller = head [Seller a b c|Seller a b c <- sellers, a == newSellerIdent] 
                                                       irrelevantSellers = [Seller a b c|Seller a b c <- sellers, a /= newSellerIdent] 
                                                       relevantSellerGoods = (\(Seller a b c) -> b) relevantSeller
                                                       goodIdent = (\(SellerU a (b,c,d,e)) -> (b,c,d)) originalSeller
                                                       relevantReleavntSellerGood = head [b|b<-relevantSellerGoods,b == goodIdent]
                                                       irrelevantRelevantSellerGoods = [b|b<-relevantSellerGoods,b /= goodIdent]
                                                       newGood = (\(SellerU a (b,c,d,e)) -> (b,c,d)) updatedSeller
                                                       newGoods = [newGood] ++ irrelevantRelevantSellerGoods 
                                                       oldSellerMoney = (\(Seller a b c) -> c) relevantSeller  
                                                       newSeller = (Seller newSellerIdent newGoods (oldSellerMoney+moneyToSeller))
                                                       newSellers = irrelevantSellers ++ [newSeller]
                                                   in priceFix (updatedBuyer:inactiveBuyers) newSellers result

outcome = priceFix testbuyers testsellers Empty                                  

main :: IO() 
main = do
putStrLn ("Buyers: "++show testbuyers)
putStrLn ("Sellers: "++show testsellers)
putStrLn ("Resulting Market Outcome: "++show outcome)









module PFixerSingleGood where

import Data.List

--WORK FROM CLASS TEMPORARILY COMMENTED OUT 
--data Util = Integer
--data Money = Double
--data Goods = Price::Money Quantity::Integer
--data Buyer = Util CashOnHand::Money [Goods]
--data Seller = MinPrice::Money [Goods]
--data Market = [Buyer] [Seller]
--priceFix :: Market a -> Market a
--calcUtilPerMoney :: Seller -> Buyer -> Goods -> [(Goods, Util)]
--buy :: (Goods, Util) -> Buyer -> Seller -> Market

--TEST IMPLEMENTATION FOR MARKET WITH ONLY 1 GOOD 
data MarketParticipant = Seller Float Float | Buyer Float Float Float deriving (Show, Ord, Eq)
data Market = Participants [MarketParticipant] | Empty deriving (Show, Ord, Eq)

--data Seller a = QP Double Double --(quantity of single good, price per unit of single good)
--data Buyer b = UMU Double Double Double --(utility from each unit of single good, money to use in market, units held)

testsellers = [Seller 4 43,Seller 6 67,Seller 2 12] 
testbuyers = [Buyer 500 10 0,Buyer 100 50 0,Buyer 2000 500 0] 


fTup (x,_,_) = x
sTup (_,x,_) = x
tTup (_,_,x) = x


matchMarketParticipants :: [(Float,Float,Float)] -> [(Float,Float,Float)] -> MarketParticipant -> (MarketParticipant,[(Float,Float,Float)])
matchMarketParticipants sellersWithInventory dudSellers activeBuyer = if sellersWithInventory == [] 
                                                                      then (activeBuyer, dudSellers) 
                                                                      else let sellerPrices = map (\(x,y,z) -> z) sellersWithInventory
                                                                               utilityPerSeller = map (\price -> buyerUtility / price) sellerPrices
                                                                               mostUtility = maximum utilityPerSeller
                                                                               activeSeller = head [x|x <- sellersWithInventory,fTup(x) == mostUtility]
                                                                               tempInactiveSellers = [x|x <- sellersWithInventory,fTup(x) /= mostUtility]
                                                                               buyerUtility = (\(Buyer x y z) -> x) activeBuyer
                                                                               buyerMoneyHeld = (\(Buyer x y z) -> y) activeBuyer
                                                                               buyerUnitsHeld = (\(Buyer x y z) -> z) activeBuyer
                                                                               sellerPrice = (\(x,y,z) -> z) activeSeller
                                                                               sellerQuantity = (\(x,y,z) -> y) activeSeller
                                                                               sellerUtility = (\(x,y,z) -> x) activeSeller
                                                                            in if buyerMoneyHeld > sellerPrice then let updateBuyer = Buyer buyerUtility (buyerMoneyHeld-sellerPrice) (buyerUnitsHeld+1)
                                                                                                                        updateActiveSeller = (sellerUtility,(sellerQuantity-1),sellerPrice)
                                                                                                                    in (updateBuyer, [updateActiveSeller] ++ tempInactiveSellers ++ dudSellers)
                                                                                                                    else matchMarketParticipants tempInactiveSellers ([activeSeller] ++ dudSellers) activeBuyer
                                                                                                          
                                                                          

priceFix:: [MarketParticipant] -> [MarketParticipant] -> Market -> Market
priceFix buyers sellers result = if buyers == [] 
                                 then let unpackedResult = (\(Participants x) -> x) result
                                          in Participants (unpackedResult++sellers)
                                 else let activeBuyer = head(buyers)
                                          inactiveBuyers = tail(buyers)
                                          sellerPrices = map (\(Seller x y) -> y) sellers
                                          sellerQuantities = map (\(Seller x y) -> x) sellers
                                          buyerUtility = (\(Buyer x y z) -> x) activeBuyer
                                          utilityPerSeller = map (\price -> buyerUtility / price) sellerPrices  
                                          sellersInfo = zip3 utilityPerSeller sellerQuantities sellerPrices
                                          sellersWithInventory = [x|x <- sellersInfo,sTup(x) > 0]
                                          sellersWithoutInventory = [x|x <- sellersInfo,sTup(x) == 0]
                                          inactiveSellers = sellersWithoutInventory
                                          transaction = matchMarketParticipants sellersWithInventory [] activeBuyer
                                          updatedBuyer = fst(transaction)
                                          updatedSellers = snd(transaction) 
                                          partialResult = map (\(x,y,z) -> Seller y z) sellersWithoutInventory
                                          unpackedInitialResult = (\(Participants x) -> x) result
                                      in if (updatedBuyer == activeBuyer) && (result == Empty) 
                                         then priceFix inactiveBuyers (map (\(x,y,z) -> Seller y z) updatedSellers)  (Participants ([activeBuyer]++partialResult))
                                         else if updatedBuyer == activeBuyer 
                                              then priceFix inactiveBuyers (map (\(x,y,z) -> Seller y z) updatedSellers)  (Participants ([activeBuyer]++partialResult++unpackedInitialResult))
                                              else priceFix (updatedBuyer:inactiveBuyers) (map (\(x,y,z) -> Seller y z) updatedSellers) result
                                 
outcome = priceFix testbuyers testsellers Empty                                  

main :: IO() 
main = do
putStrLn ("Buyers: "++show testbuyers++", Sellers: "++show testsellers++", Resulting Market Outcome: "++show outcome)





module PriceMonads where 


import Control.Monad.State

data MarketParticipant = Seller Float Float | Buyer Float Float Float deriving (Show, Ord, Eq)
data Market = Participants [MarketParticipant] | Empty deriving (Show, Ord, Eq)

instance Monad (Market a) where
    return x = Market $ \a -> Participants[x]
    Participants lst >>= f = Participants $ \a -> let (x, newMarket) = lst a
                                          (Participants l) = f x
                                      in l newMarket
    Empty >>= f = Empty
    
    
    

    
    
-}