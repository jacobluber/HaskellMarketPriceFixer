module PFixerMultGoods where

import Data.List

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
                                                                               --sellerPrices = map (\(x,y,z) -> z) sellersWithInventory
                                                                               --utilityPerSeller = map (\price -> buyerUtility / price) sellerPrices
                                                                               mostUtility = maximum utilityPerSeller
                                                                               activeSeller = head [SellerU a (b,c,d,e)|SellerU a (b,c,d,e) <- sellersWithInventory,e == mostUtility]
                                                                               tempInactiveSellers = [SellerU a (b,c,d,e)|SellerU a (b,c,d,e) <- sellersWithInventory,e /= mostUtility]
                                                                               --tempInactiveSellers = [x|x <- sellersWithInventory,fTup(x) /= mostUtility]
                                                                               --buyerUtility = (\(Buyer x y z) -> x) activeBuyer
                                                                               buyerMoneyHeld = (\(Buyer x y z) -> z) activeBuyer
                                                                               --buyerUnitsHeld = (\(Buyer x y z) -> y) activeBuyer
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
                                                                                                                        --updateBuyer = Buyer buyerUtility (buyerMoneyHeld-sellerPrice) (buyerUnitsHeld+1)
                                                                                                                        updateActiveSeller = SellerU sellerIdent (sellerGood, sellerQuantity-1, sellerPrice, sellerUtility) 
                                                                                                                        --updateActiveSeller = (sellerUtility,(sellerQuantity-1),sellerPrice)
                                                                                                                    in (updateBuyer, updateActiveSeller, activeSeller, sellerPrice)
                                                                                                                    --in (updateBuyer, [updateActiveSeller] ++ tempInactiveSellers ++ dudSellers)
                                                                                                                    else matchMarketParticipants tempInactiveSellers ([activeSeller] ++ dudSellers) activeBuyer
                                                                                                          
                                                                          



priceFix:: [MarketParticipant] -> [MarketParticipant] -> Market -> Market
priceFix buyers sellers result = if buyers == [] 
                                 then let unpackedResult = (\(Participants x) -> x) result
                                          in Participants (unpackedResult++sellers)
                                 else let (Buyer identifier goods money) = head(buyers)
                                          inactiveBuyers = tail(buyers)
                                          buyerUtility = map (\(a,b,c) -> c) goods --[50.0,25.0,10.0]
                                          sellerVals = map (\(Seller a b c) -> map (\b -> (a,b)) b) sellers
                                          specialSellerInput = concat (map (\t -> zip buyerUtility t) sellerVals)
                                          generateSpecialSellers = map (\(a, (b, (c, d, e))) -> SellerU b (c,d,e,a/e)) specialSellerInput
                                          specialSellersWithInventory = [SellerU a (b,c,d,e)| SellerU a (b,c,d,e) <- generateSpecialSellers, c>0]
                                          specialSellersWithoutInventory = [SellerU a (b,c,d,e)| SellerU a (b,c,d,e) <- generateSpecialSellers, c==0]
                                          --sellersOfferingRelevantGoods = [x|x <- sellers, fTup(x) == fTup(l)]                       
                                          --sellerPrices = map (\(Seller x y) -> y) sellers
                                          --sellerQuantities = map (\(Seller x y) -> x) sellers
                                          --buyerUtility = (\(Buyer x y z) -> x) activeBuyer
                                          --utilityPerSeller = map (\price -> buyerUtility / price) sellerPrices  
                                          --sellersInfo = zip3 utilityPerSeller sellerQuantities sellerPrices
                                          --sellersWithInventory = [x|x <- sellersInfo,sTup(x) > 0]
                                          --sellersWithoutInventory = [x|x <- sellersInfo,sTup(x) == 0]
                                          --inactiveSellers = sellersWithoutInventory
                                          transaction = matchMarketParticipants specialSellersWithInventory [] (Buyer identifier goods money)
                                          --updatedBuyer = fst(transaction)
                                          --updatedSellers = snd(transaction)
                                          updatedBuyer = fTup(transaction)
                                          updatedSeller = sTup(transaction)
                                          originalSeller = tTup(transaction)
                                          moneyToSeller = frTup(transaction)
                                          --partialResult = map (\(x,y,z) -> Seller y z) sellersWithoutInventory
                                          --(updateBuyer, updateActiveSeller, activeSeller, sellerPrice)
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
                                                       --sellerIdent = (\(Seller a b c) -> a) sellers 
                                                       --sellerGoods = (\(Seller a b c) -> b) sellers
                                                       --sellerMoney = (\(Seller a b c) -> c) sellers 
                                                       --                     sellerQuantity = (\SellerU a (b,c,d,e) -> c) activeSeller
                                                                              -- sellerUtility = (\SellerU a (b,c,d,e) -> e) activeSeller
                                                                              -- sellerGood = (\SellerU a (b,c,d,e) -> b) activeSeller
                                              --priceFix (updatedBuyer:inactiveBuyers) (map (\(x,y,z) -> Seller y z) updatedSellers) result
                                 
outcome = priceFix testbuyers testsellers Empty                                  

main :: IO() 
main = do
putStrLn ("Buyers: "++show testbuyers)
putStrLn ("Sellers: "++show testsellers)
putStrLn ("Resulting Market Outcome: "++show outcome)