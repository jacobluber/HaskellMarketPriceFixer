module PFixerMultGoodDimMargUtil where

data MarketParticipant = Seller String [(String, Float, Float)] Float | Buyer String [(String, Float, (Float,Float,Float))] Float Float deriving (Show, Ord, Eq)
data EndMarket = EndParticipants Float Float [MarketParticipant] deriving (Show, Ord, Eq)
data Market = Participants [MarketParticipant] | Empty deriving (Show, Ord, Eq)
data SpecialSeller = SellerU String (String, Float, Float, Float) | NoSeller deriving (Show, Ord, Eq)
testsellers = [Seller "A" [("a",4,1),("b",7,2),("c",5,3)] 0,Seller "B" [("a",1,3),("b",8,2),("c",2,1)] 0,Seller "C" [("a",0,2),("b",4,2),("c",1,2)] 0] 
testbuyers = [Buyer "Z" [("a",0,(4,2,(4*100)/(0+2))),("b",0,(2,3,(2*100)/(0+3))),("c",0,(6,1,(6*100)/(0+1)))] 100 0, Buyer "Y" [("a",0,(5,4,(5*50)/(0+4))),("b",0,(4,2,(4*50)/(0+5))),("c",0,(2,1,(2*50)/(0+2)))] 50 0,Buyer "X" [("a",0,(5,1,(5*250)/(0+1))),("b",0,(3,0.9,(3*250)/(0+0.9))),("c",0,(1,2,(4*250)/(0+2)))] 250 0] 

--SpecialSeller SellerStringIdentifier (GoodIdentifier,UnitsOfGoodHeld,PriceChargedForGood,UtilityPerDollarForActiveBuyer) 
--Seller SellerStringIdentifier [(GoodIdentifier,UnitsOfGoodHeld,PricePaidForGood)] MoneyHeld
--Buyer BuyerStringIdentifier [(GoodIdentifier,UnitsOfGoodHeld,(WealthDeccrementer,QuantityIncrementer,MarginalUtilityFunction)] MoneyHeld AccumulatedUtility 
--EndMarket TotalSellerProfit TotalBuyerUtility [MarketParticipant]

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
                                                                               buyerMoneyHeld = (\(Buyer x y z w) -> z) activeBuyer
                                                                               sellerPrice = (\(SellerU a (b,c,d,e)) -> d) activeSeller
                                                                               sellerQuantity = (\(SellerU a (b,c,d,e)) -> c) activeSeller
                                                                               sellerUtility = (\(SellerU a (b,c,d,e)) -> e) activeSeller
                                                                               sellerGood = (\(SellerU a (b,c,d,e)) -> b) activeSeller
                                                                               sellerIdent =  (\(SellerU a (b,c,d,e)) -> a) activeSeller
                                                                            in if (buyerMoneyHeld > sellerPrice) && (mostUtility > 0) then let oldBuyerMoney = (\(Buyer a b c d) -> c) activeBuyer 
                                                                                                                                               newMoney = oldBuyerMoney-sellerPrice
                                                                                                                                               relevantBuyerGoods = (\(Buyer a b c d) -> b) activeBuyer
                                                                                                                                               goodIdent = (\(SellerU a (b,c,d,e)) -> b) activeSeller
                                                                                                                                               relevantRelevantBuyerGood = head [(a,b,c)|(a,b,c) <-relevantBuyerGoods,a == goodIdent]
                                                                                                                                               irrelevantRelevantBuyerGoods = [(a,b,c)|(a,b,c) <- relevantBuyerGoods,a /= goodIdent]
                                                                                                                                               utilFunc = (\(a,b,c) -> c) relevantRelevantBuyerGood
                                                                                                                                               wealthDecrementer = (\(a,b,c) -> a) utilFunc
                                                                                                                                               quantityIncrementer = (\(a,b,c) -> b) utilFunc 
                                                                                                                                               utilityGained = (\(a,b,c) -> c) utilFunc
                                                                                                                                               quantityOriginallyHeld = (\(a,b,c) -> b) relevantRelevantBuyerGood                                                                                        
                                                                                                                                               newUtilFunc = (wealthDecrementer, quantityIncrementer, ((wealthDecrementer*newMoney)/(quantityIncrementer+(quantityOriginallyHeld+1))))
                                                                                                                                               newGood = (\(a,b,c) -> (a,b+1,newUtilFunc)) relevantRelevantBuyerGood
                                                                                                                                               newGoods = [newGood] ++ irrelevantRelevantBuyerGoods                                             
                                                                                                                                               buyerIdent = (\(Buyer a b c d) -> a) activeBuyer
                                                                                                                                               oldAccum = (\(Buyer a b c d) -> d) activeBuyer 
                                                                                                                                               updateBuyer = (Buyer buyerIdent newGoods newMoney (oldAccum+utilityGained))                                                                                                                                                                                                                                                                                                            
                                                                                                                                               updateActiveSeller = SellerU sellerIdent (sellerGood, sellerQuantity-1, sellerPrice, sellerUtility)                                                                                                                        
                                                                                                                                            in (updateBuyer, updateActiveSeller, activeSeller, sellerPrice)                                                                                                                   
                                                                                                                                            else matchMarketParticipants tempInactiveSellers ([activeSeller] ++ dudSellers) activeBuyer                                                                                                                                                                                 

priceFix:: [MarketParticipant] -> [MarketParticipant] -> Market -> EndMarket
priceFix buyers sellers result = if buyers == [] 
                                 then let unpackedResult = (\(Participants x) -> x) result
                                          accumulatedBuyerUtilities = map (\(Buyer a b c d) -> d) unpackedResult
                                          accumulatedBuyerUtility = foldl (+) 0 accumulatedBuyerUtilities
                                          accumulatedSellerMoney = map (\(Seller a b c) -> c) sellers
                                          accumulatedSellerTotalMoney = foldl (+) 0 accumulatedSellerMoney 
                                          in EndParticipants accumulatedSellerTotalMoney accumulatedBuyerUtility (unpackedResult++sellers)
                                 else let (Buyer identifier goods money accumUtility) = head(buyers)
                                          inactiveBuyers = tail(buyers)
                                          utilVals = map(\(a,b,c) -> c) goods
                                          buyerUtility = map (\(a,b,c) -> c) utilVals
                                          sellerVals = map (\(Seller a b c) -> map (\b -> (a,b)) b) sellers
                                          specialSellerInput = concat (map (\t -> zip buyerUtility t) sellerVals)
                                          generateSpecialSellers = map (\(a, (b, (c, d, e))) -> SellerU b (c,d,e,a/e)) specialSellerInput
                                          specialSellersWithInventory = [SellerU a (b,c,d,e)| SellerU a (b,c,d,e) <- generateSpecialSellers, c>0]
                                          specialSellersWithoutInventory = [SellerU a (b,c,d,e)| SellerU a (b,c,d,e) <- generateSpecialSellers, c==0]
                                          transaction = matchMarketParticipants specialSellersWithInventory [] (Buyer identifier goods money accumUtility)
                                          updatedBuyer = fTup(transaction)
                                          updatedSeller = sTup(transaction)
                                          originalSeller = tTup(transaction)
                                          moneyToSeller = frTup(transaction)
                                          unpackedInitialResult = (\(Participants x) -> x) result
                                      in if (updatedBuyer == (Buyer identifier goods money accumUtility)) && (result == Empty) 
                                         then priceFix inactiveBuyers sellers (Participants ([(Buyer identifier goods money accumUtility)]))
                                         else if updatedBuyer == (Buyer identifier goods money accumUtility)
                                              then priceFix inactiveBuyers sellers  (Participants ([(Buyer identifier goods money accumUtility)]++unpackedInitialResult))
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

generateSellerInput :: MarketParticipant -> [[Float]]
generateSellerInput x = let goods = (\(Seller a b c) -> b) x
                            purchasePrices = map (\(a,b,c) -> c) goods
                            range = (map (\x -> [x..(x*2)]) purchasePrices)
                            result = [[x,y,z]|x<-head(range),y<-head(tail(range)),z<-last(range)]
                        in result        

generateSellerInputs x =  map generateSellerInput x 


generateFinalInputs x = let input = generateSellerInputs x
                            result = [[x,y,z]|x<-head(input),y<-head(tail(input)),z<-last(input)]
                        in result

sellerval = (\(Seller a [(b,c,d),(e,f,g),(h,i,j)] k) -> (Seller a [(b,c,-1),(e,f,-2),(h,i,-3)] k))
sellervals = map sellerval testsellers
input = map (\a -> zip a sellervals) (generateFinalInputs testsellers)
sellers = map (\([x,y,z],Seller a [(b,c,-1),(e,f,-2),(h,i,-3)] k) -> Seller a [(b,c,x),(e,f,y),(h,i,z)] k) 
finalSellers = map sellers input
marketOutcomes = map (\b -> priceFix testbuyers b Empty) finalSellers
mostProfit = maximum (map (\(EndParticipants x y z) -> x) marketOutcomes)
mostUtility = maximum (map (\(EndParticipants x y z) -> y) marketOutcomes)
marketProfits = [(EndParticipants x y z)|(EndParticipants x y z)<-marketOutcomes, x==mostProfit]
marketUtility = [(EndParticipants x y z)|(EndParticipants x y z)<-marketOutcomes, y==mostUtility]
--outcome = priceFix testbuyers testsellers Empty                                  

main :: IO() 
main = do
putStrLn ("For the market of buyers and sellers represented by: ")
putStrLn ("Buyers: "++show testbuyers)
putStrLn ("Sellers: "++show testsellers)
putStrLn (" ")
putStrLn ("The algorithm has tested "++show (length marketOutcomes)++" different market outcomes.")
putStrLn ("Of these outcomes, the one(s) that generate(s) the most seller profit("++(show mostProfit)++") is/are: ")
putStrLn (show marketProfits)
putStrLn (" ")
putStrLn ("Of these outcomes, the one(s) that generate(s) the most buyer utility("++(show mostUtility)++") is/are: ")
putStrLn (show marketUtility)


--putStrLn ("Resulting Market Outcome: "++show outcome)


