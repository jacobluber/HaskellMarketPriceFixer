module PFixerMultGoodDimMargUtil where

import TupleOps
import DataType

sellerUtils :: [Offer] -> [Float]
sellerUtils sellers = map (\(OfferInfo sellerInfo goodInfo) -> (frTup goodInfo)) sellers

generateNewGood :: (Num t1, Num t2) => (String, t1, t2, t2) -> (String, t1, t2, t2)
generateNewGood input = (\(goodIdentifier,unitsHeld,marginalUtility,utilityDecrementer) -> (goodIdentifier,unitsHeld+1,marginalUtility-utilityDecrementer,utilityDecrementer)) input

generateActiveSeller :: [Offer] -> Offer
generateActiveSeller sellers = head [OfferInfo sellerId (goodId,unitsGoodHeld,priceCharged,utilityPerDollar)|OfferInfo sellerId (goodId,unitsGoodHeld,priceCharged,utilityPerDollar) <- sellers,utilityPerDollar == (maximum (sellerUtils sellers))]

generateTempInactiveSellers :: [Offer] -> [Offer]
generateTempInactiveSellers sellers = [OfferInfo sellerId (goodId,unitsGoodHeld,priceCharged,utilityPerDollar)|OfferInfo sellerId (goodId,unitsGoodHeld,priceCharged,utilityPerDollar) <- sellers,utilityPerDollar /= (maximum (sellerUtils sellers))]

generateBuyerGood :: MarketParticipant -> Offer -> (String, Float, Float, Float)
generateBuyerGood buyer seller = head [(goodIdentifier,unitsHeld,marginalUtility,utilityDecrementer)|(goodIdentifier,unitsHeld,marginalUtility,utilityDecrementer) <- (buyerGoodInfo buyer),goodIdentifier == (fTup (goodInfo seller))]  

generateBuyerGoods :: MarketParticipant -> Offer -> [(String, Float, Float, Float)]
generateBuyerGoods buyer seller = [(goodIdentifier,unitsHeld,marginalUtility,utilityDecrementer)|(goodIdentifier,unitsHeld,marginalUtility,utilityDecrementer) <- (buyerGoodInfo buyer),goodIdentifier /= (fTup (goodInfo seller))]

matchMarketParticipants :: [Offer] -> [Offer] -> MarketParticipant -> (MarketParticipant,Offer,Offer,Float)
matchMarketParticipants sellersWithInventory dudSellers activeBuyer = 
 if sellersWithInventory == [] 
 then (activeBuyer, NoSeller, NoSeller, 0) 
 else let activeSeller = generateActiveSeller sellersWithInventory
          tempInactiveSellers = generateTempInactiveSellers sellersWithInventory
      in if ((moneyHeld activeBuyer) > (tTup (goodInfo activeSeller))) && ((maximum (sellerUtils sellersWithInventory)) > 0) 
         then let buyerGood = generateBuyerGood activeBuyer activeSeller               
                  buyerGoods = generateBuyerGoods activeBuyer activeSeller
                  newGoods = [generateNewGood buyerGood] ++ buyerGoods                                             
                  updateBuyer = Buyer (buyerId activeBuyer) newGoods ((moneyHeld activeBuyer)-(tTup (goodInfo activeSeller))) ((accumulatedUtility activeBuyer)+(tTup buyerGood))                                                                                                                                                                                                                                                                                                          
                  updateActiveSeller = OfferInfo (sellerInfo activeSeller ) ((fTup (goodInfo activeSeller)), ((sTup (goodInfo activeSeller))-1), (tTup (goodInfo activeSeller)), (frTup (goodInfo activeSeller)))                                                                                                                        
              in (updateBuyer, updateActiveSeller, activeSeller, (tTup (goodInfo activeSeller)))                                                                                                                   
         else matchMarketParticipants tempInactiveSellers ([activeSeller] ++ dudSellers) activeBuyer                                                                                                                                                                                 

--generateEndMarket sellers result = let unpackedResult = (\(Participants x) -> x) result
                  

priceFix:: [MarketParticipant] -> [MarketParticipant] -> Market -> EndMarket
priceFix buyers sellers result = if buyers == [] 
                                 then let unpackedResult = (\(Participants x) -> x) result
                                          accumulatedBuyerUtilities = map (\(Buyer a b c d) -> d) unpackedResult
                                          accumulatedBuyerUtility = foldl (+) 0 accumulatedBuyerUtilities
                                          accumulatedSellerMoney = map (\(Seller a b c) -> c) sellers
                                          accumulatedSellerTotalMoney = foldl (+) 0 accumulatedSellerMoney 
                                       in EndParticipants accumulatedSellerTotalMoney accumulatedBuyerUtility (unpackedResult++sellers)
                                
                                 else let buyer = head(buyers)
                                          --(Buyer identifier goods money accumUtility) = head(buyers)
                                          inactiveBuyers = tail(buyers)
                                          utilVals = map (\(a,b,c,d) -> c) (buyerGoodInfo buyer)
                                          --buyerUtility = map (\(a,b,c) -> c) utilVals
                                          sellerVals = map (\(Seller a b c) -> map (\b -> (a,b)) b) sellers
                                          --specialSellerInput = concat (map (\t -> zip buyerUtility t) sellerVals)
                                          specialSellerInput = concat (map (\t -> zip utilVals t) sellerVals)
                                          generateSpecialSellers = map (\(a, (b, (c, d, e))) -> OfferInfo b (c,d,e,a/e)) specialSellerInput
                                          specialSellersWithInventory = [OfferInfo a (b,c,d,e)| OfferInfo a (b,c,d,e) <- generateSpecialSellers, c>0]
                                          specialSellersWithoutInventory = [OfferInfo a (b,c,d,e)| OfferInfo a (b,c,d,e) <- generateSpecialSellers, c==0]
                                          transaction = matchMarketParticipants specialSellersWithInventory [] buyer
                                          updatedBuyer = fTup(transaction)
                                          updatedSeller = sTup(transaction)
                                          originalSeller = tTup(transaction)
                                          moneyToSeller = frTup(transaction)
                                          unpackedInitialResult = (\(Participants x) -> x) result
                                      in if ((updatedBuyer == buyer) && (result == Empty)) 
                                         then priceFix inactiveBuyers sellers (Participants ([buyer]))
                                         else if updatedBuyer == buyer
                                              then priceFix inactiveBuyers sellers  (Participants ([buyer]++unpackedInitialResult))
                                              else let newSellerIdent =  (\(OfferInfo a (b,c,d,e)) -> a) originalSeller
                                                       relevantSeller = head [Seller a b c|Seller a b c <- sellers, a == newSellerIdent] 
                                                       irrelevantSellers = [Seller a b c|Seller a b c <- sellers, a /= newSellerIdent] 
                                                       relevantSellerGoods = (\(Seller a b c) -> b) relevantSeller
                                                       goodIdent = (\(OfferInfo a (b,c,d,e)) -> (b,c,d)) originalSeller
                                                       relevantReleavntSellerGood = head [b|b<-relevantSellerGoods,b == goodIdent]
                                                       irrelevantRelevantSellerGoods = [b|b<-relevantSellerGoods,b /= goodIdent]
                                                       newGood = (\(OfferInfo a (b,c,d,e)) -> (b,c,d)) updatedSeller
                                                       newGoods = [newGood] ++ irrelevantRelevantSellerGoods 
                                                       oldSellerMoney = (\(Seller a b c) -> c) relevantSeller  
                                                       newSeller = (Seller newSellerIdent newGoods (oldSellerMoney+moneyToSeller))
                                                       newSellers = irrelevantSellers ++ [newSeller]
                                                   in priceFix (updatedBuyer:inactiveBuyers) newSellers result



generateSellerInput :: MarketParticipant -> [[Float]]
generateSellerInput x = let goods = (\(Seller a b c) -> b) x
                            purchasePrices = map (\(a,b,c) -> c) goods
                            range = (map (\x -> [x..(x*2)]) purchasePrices)
                            result = sequence range
                        in result        

generateSellerInputs x =  map generateSellerInput x 

generateFinalInputs x = let input = generateSellerInputs x
                            result = sequence input 
                        in result
                                               
getIds a = sellerId a

sellerIds = map getIds testsellers

getGoods a = map (\(a,b,c) -> (a,b,-1)) (sellerGoodInfo a)  

sellervals = map getGoods testsellers

input = map (\a -> zip a sellervals) (generateFinalInputs testsellers)

createSellerGoods :: ([Float], [(String, Float, Integer)]) -> [(String,Float,Float)]
createSellerGoods ([], _) = []
createSellerGoods (_,[]) = []
createSellerGoods (x:xs, y:ys) = [(fTup' y,sTup' y,x)] ++ createSellerGoods (xs, ys)
createSellersGoods a = map (map createSellerGoods) a 

almostSellers a = map (zip sellerIds) (createSellersGoods a) 

createSeller (x,y) = Seller x y 0

createSellers a = map (map createSeller) (almostSellers a) 

marketOutcomes = map (\b -> priceFix testbuyers b Empty) (createSellers input)

mostProfit = maximum (map (\(EndParticipants x y z) -> x) marketOutcomes)

mostUtility = maximum (map (\(EndParticipants x y z) -> y) marketOutcomes)

marketProfits = [(EndParticipants x y z)|(EndParticipants x y z)<-marketOutcomes, x==mostProfit]

marketUtility = [(EndParticipants x y z)|(EndParticipants x y z)<-marketOutcomes, y==mostUtility]
                              

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


