module PFixer where


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
data MarketParticipant = Seller Int Int | Buyer Int Int Int deriving (Show, Ord, Eq)
data Market = Participants [MarketParticipant] deriving (Show, Ord, Eq)

--data Seller a = QP Int Int --(quantity of single good, price per unit of single good)
--data Buyer b = UMU Int Int Int --(utility from each unit of single good, money to use in market, units held)

val testsellers = [Seller 4 43,Seller 6 67,Seller 2 12] 
val testbuyers = [Buyer 500 10 0,Buyer 100 50 0,Buyer 2000 500 0] 

priceFix:: [MarketParticipant] -> [MarketParticipant] -> Market
priceFix buyers sellers result = if buyers == [] then result else 
                                 let buyerUtility = (\(Buyer x y z) -> x) head(buyers)
                                     sellerPrices = map (\(Seller x y) -> y) testsellers  
                                     sellerQuantity = map (\(Seller x y) -> x) testsellers
                                     utilityPerSeller = map (\price -> price/BuyerUtility) sellerPrices    
                                     max = maximum utilityPerSeller 
                                     quantutils = zip utilityPerSeller sellerQuantity 
                                     bestutil = filter (max,_) quantutils
                                     if snd(bestutil) 
                                     
                                     
                                                                 
                                     



