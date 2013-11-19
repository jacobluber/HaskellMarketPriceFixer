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

                                     
                                                                 
                                     



