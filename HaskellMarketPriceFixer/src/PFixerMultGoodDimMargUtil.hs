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

generateEndMarket :: [MarketParticipant] -> Market -> EndMarket
generateEndMarket sellers result = 
 let unpackedResult = (\(Participants x) -> x) result
     accumulatedBuyerUtilities = map (\(Buyer a b c d) -> d) unpackedResult
     accumulatedBuyerUtility = foldl (+) 0 accumulatedBuyerUtilities
     accumulatedSellerMoney = map (\(Seller a b c) -> c) sellers
     accumulatedSellerTotalMoney = foldl (+) 0 accumulatedSellerMoney                 
  in EndParticipants accumulatedSellerTotalMoney accumulatedBuyerUtility (unpackedResult++sellers)                

generateUtilVals :: [(t, t1, b, t3)] -> [b]
generateUtilVals = map tTup 

generateSellerVals :: [MarketParticipant] -> [[(String, (String, Float, Float))]]
generateSellerVals = map (\(Seller a b c) -> map (\b -> (a,b)) b)

generateOfferInput :: [a] -> [[b]] -> [(a, b)]
generateOfferInput utils sellers = concat (map (\t -> zip utils t) sellers)

generateOffers :: [(Float, (String, (String, Float, Float)))] -> [Offer]
generateOffers input = map (\(a, (b, (c, d, e))) -> OfferInfo b (c,d,e,a/e)) input

generateValidOffers :: [Offer] -> [Offer]
generateValidOffers offers = [OfferInfo a (b,c,d,e)| OfferInfo a (b,c,d,e) <- offers, c>0]

generateInvalidOffers :: [Offer] -> [Offer]
generateInvalidOffers offers = [OfferInfo a (b,c,d,e)| OfferInfo a (b,c,d,e) <- offers, c==0]

generateUnpackedInitialResult :: Market -> [MarketParticipant]
generateUnpackedInitialResult = (\(Participants x) -> x)

makeTransaction :: [(t, t1, Float, t3)] -> [MarketParticipant] -> [MarketParticipant] -> (MarketParticipant, Offer, Offer, Float)
makeTransaction utilValsInput sellers buyers = 
 let utilVals = generateUtilVals utilValsInput
     sellerVals = generateSellerVals sellers
     offerInput = generateOfferInput utilVals sellerVals
     offers = generateOffers offerInput
     validOffers = generateValidOffers offers
     invalidOffers = generateInvalidOffers offers
 in matchMarketParticipants validOffers [] (head(buyers))

generateNewSellerIdent :: (t, t1, Offer, t3) -> String     
generateNewSellerIdent transaction = (\(OfferInfo a (b,c,d,e)) -> a) (tTup(transaction))

generateRelevantSeller :: [MarketParticipant] -> String -> MarketParticipant
generateRelevantSeller sellers newseller = head [Seller a b c|Seller a b c <- sellers, a == newseller] 

generateIrrelevantSellers :: [MarketParticipant] -> String -> [MarketParticipant]
generateIrrelevantSellers sellers newseller = [Seller a b c|Seller a b c <- sellers, a /= newseller] 

generateRelevantSellerGoods :: MarketParticipant -> [(String, Float, Float)]
generateRelevantSellerGoods = (\(Seller a b c) -> b)

generategoodIdent :: Offer -> (String, Float, Float)
generategoodIdent = (\(OfferInfo a (b,c,d,e)) -> (b,c,d))

generateIrrelevantRelevantSellerGoods :: Eq t => [t] -> t -> [t]
generateIrrelevantRelevantSellerGoods relevantSellerGoods goodIdent = [b|b<-relevantSellerGoods,b /= goodIdent]

generateNewGoods :: (t, Offer, t2, t3) -> [(String, Float, Float)] -> [(String, Float, Float)]
generateNewGoods transaction irrelevantRelevantSellerGoods =
 let newGood = (\(OfferInfo a (b,c,d,e)) -> (b,c,d)) (sTup(transaction))
 in [newGood] ++ irrelevantRelevantSellerGoods 

generateNewSellers :: MarketParticipant -> [Char] -> (t1, Offer, t2, Float) -> [(String, Float, Float)] -> t -> [MarketParticipant] -> [MarketParticipant] 
generateNewSellers relevantSeller newSellerIdent transaction irrelevantRelevantSellerGoods oldSellerMoney irrelevantSellers = 
 let oldSellerMoney = (\(Seller a b c) -> c) relevantSeller 
     newSeller = (Seller newSellerIdent (generateNewGoods transaction irrelevantRelevantSellerGoods) (oldSellerMoney+(frTup(transaction)))) 
 in irrelevantSellers ++ [newSeller]

recursivePriceFix :: (MarketParticipant, Offer, Offer, Float) -> [MarketParticipant] -> [MarketParticipant] -> Market -> EndMarket
recursivePriceFix transaction sellers buyers result = 
 let newSellerIdent =  generateNewSellerIdent transaction
     relevantSeller = generateRelevantSeller sellers newSellerIdent 
     irrelevantSellers = generateIrrelevantSellers sellers newSellerIdent
     relevantSellerGoods = generateRelevantSellerGoods relevantSeller
     goodIdent = generategoodIdent (tTup(transaction))
     irrelevantRelevantSellerGoods = generateIrrelevantRelevantSellerGoods relevantSellerGoods goodIdent
     oldSellerMoney = (\(Seller a b c) -> c) relevantSeller 
 in priceFix ((fTup(transaction)):(tail(buyers))) (generateNewSellers relevantSeller newSellerIdent transaction irrelevantRelevantSellerGoods oldSellerMoney irrelevantSellers) result
 
priceFix:: [MarketParticipant] -> [MarketParticipant] -> Market -> EndMarket
priceFix buyers sellers result = 
 if buyers == [] 
 then generateEndMarket sellers result
 else let transaction = makeTransaction (buyerGoodInfo (head(buyers))) sellers buyers
      in if (((fTup(transaction)) == (head(buyers))) && (result == Empty)) 
         then priceFix (tail(buyers)) sellers (Participants ([(head(buyers))]))
         else if (fTup(transaction)) == (head(buyers))
              then priceFix (tail(buyers)) sellers  (Participants ([(head(buyers))]++(generateUnpackedInitialResult result)))
              else recursivePriceFix transaction sellers buyers result


generateSellerInput :: MarketParticipant -> [[Float]]
generateSellerInput x = 
 let goods = (\(Seller a b c) -> b) x
     purchasePrices = map (\(a,b,c) -> c) goods
     range = (map (\x -> [x..(x*2)]) purchasePrices)
     result = sequence range
 in result        

generateSellerInputs :: [MarketParticipant] -> [[[Float]]]
generateSellerInputs x =  map generateSellerInput x 

generateFinalInputs :: [MarketParticipant] -> [[[Float]]]
generateFinalInputs x = 
 let input = generateSellerInputs x
     result = sequence input 
 in result

getIds :: MarketParticipant -> String                                              
getIds a = sellerId a

sellerIds :: [String]
sellerIds = map getIds testsellers

getGoods :: Num t => MarketParticipant -> [(String, Float, t)]
getGoods a = map (\(a,b,c) -> (a,b,-1)) (sellerGoodInfo a)  

sellervals :: Num t => [MarketParticipant] -> [[(String, Float, t)]]
sellervals a= map getGoods a

input :: [[([Float], [(String, Float, Integer)])]]
input = map (\a -> zip a (sellervals currentsellerinput)) (generateFinalInputs currentsellerinput)

createSellerGoods :: ([Float], [(String, Float, Integer)]) -> [(String,Float,Float)]
createSellerGoods ([], _) = []
createSellerGoods (_,[]) = []
createSellerGoods (x:xs, y:ys) = [(fTup' y,sTup' y,x)] ++ createSellerGoods (xs, ys)
createSellersGoods a = map (map createSellerGoods) a 

almostSellers :: [[([Float], [(String, Float, Integer)])]] -> [[(String, [(String, Float, Float)])]]
almostSellers a = map (zip sellerIds) (createSellersGoods a) 

createSeller :: (String, [(String, Float, Float)]) -> MarketParticipant
createSeller (x,y) = Seller x y 0

createSellers :: [[([Float], [(String, Float, Integer)])]] -> [[MarketParticipant]]
createSellers a = map (map createSeller) (almostSellers a) 

marketOutcomes :: [EndMarket]
marketOutcomes = map (\b -> priceFix currentbuyerinput b Empty) (createSellers input)

mostProfit :: Float
mostProfit = maximum (map (\(EndParticipants x y z) -> x) marketOutcomes)

mostUtility :: Float
mostUtility = maximum (map (\(EndParticipants x y z) -> y) marketOutcomes)

marketProfits :: [EndMarket]
marketProfits = [(EndParticipants x y z)|(EndParticipants x y z)<-marketOutcomes, x==mostProfit]

marketUtility :: [EndMarket]
marketUtility = [(EndParticipants x y z)|(EndParticipants x y z)<-marketOutcomes, y==mostUtility]

currentbuyerinput = testbuyers
currentsellerinput = testsellers
                              

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




