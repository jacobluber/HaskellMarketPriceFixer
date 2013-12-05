module Main where

import TupleOps
import DataType

{-
The current iteration of the program does not support user input from the command line.
Thus, currentbuyerinput refers to buyers and currentsellerinput refers to sellers.
Any number of buyers, sellers, and goods can be used.
A sample input of sellers and buyers is provided in the DataType module.
The value pricemultiplier refers to what every default price will be multiplied by to 
generate the maximum of the range of attempted prices for each good. 
Stylized output can be generated by loading the program in GHCi and calling "main". 
-}

currentbuyerinput = testbuyers
currentsellerinput = testsellers
pricemultiplier = 2

sellerUtils :: [Offer] -> [Float]
sellerUtils sellers = map (\(OfferInfo sellerInfo goodInfo) -> (frTup goodInfo)) sellers

generateNewGood :: (Num t1, Num t2) => (String, t1, t2, t2) -> (String, t1, t2, t2)
generateNewGood input = (\(goodIdentifier,unitsHeld,marginalUtility,utilityDecrementer) -> 
        (goodIdentifier,unitsHeld+1,marginalUtility-utilityDecrementer,utilityDecrementer)) input

generateActiveSeller :: [Offer] -> Offer
generateActiveSeller sellers = head [ offer | offer@(OfferInfo sellerId (_,_,_,upd)) <- sellers, upd == (maximum (sellerUtils sellers))]

generateTempInactiveSellers :: [Offer] -> [Offer]
generateTempInactiveSellers sellers = [ offer | offer@(OfferInfo sellerId (_,_,_,upd)) <- sellers, upd /= (maximum (sellerUtils sellers))]

generateBuyerGood :: MarketParticipant -> Offer -> (String, Float, Float, Float)
generateBuyerGood buyer seller = head [ ret | ret@(goodID,_,_,_) <- (buyerGoodInfo buyer), goodID == (fTup (goodInfo seller))]  

generateBuyerGoods :: MarketParticipant -> Offer -> [(String, Float, Float, Float)]
generateBuyerGoods buyer seller = [ ret | ret@(goodID,_,_,_) <- (buyerGoodInfo buyer), goodID /= (fTup (goodInfo seller))]

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
                  updateBuyer = Buyer (buyerId activeBuyer) newGoods 
                        ((moneyHeld activeBuyer)-(tTup (goodInfo activeSeller))) 
                        ((accumulatedUtility activeBuyer)+(tTup buyerGood))                                                                                                                                                                                                                                                                                                          
                  updateActiveSeller = OfferInfo 
                        (sellerInfo activeSeller ) 
                        ((fTup (goodInfo activeSeller)), 
                        ((sTup (goodInfo activeSeller))-1), 
                        (tTup (goodInfo activeSeller)), 
                        (frTup (goodInfo activeSeller)))                                                                                                                        
              in (updateBuyer, updateActiveSeller, activeSeller, (tTup (goodInfo activeSeller)))                                                                                                                   
         else matchMarketParticipants tempInactiveSellers ([activeSeller] ++ dudSellers) activeBuyer                                                                                                                                                                                 

generateEndMarket :: [MarketParticipant] -> Market -> EndMarket
generateEndMarket sellers result = 
 let unpackedResult = (\(Participants x) -> x) result
     accumulatedBuyerUtilities = map (\(Buyer buyerID goodInfo moneyHeld accumulatedUtility) -> accumulatedUtility) unpackedResult
     accumulatedBuyerUtility = foldl (+) 0 accumulatedBuyerUtilities
     accumulatedSellerMoney = map (\(Seller sellerID goodInfo moneyHeld) -> moneyHeld) sellers
     accumulatedSellerTotalMoney = foldl (+) 0 accumulatedSellerMoney                 
  in EndParticipants accumulatedSellerTotalMoney accumulatedBuyerUtility (unpackedResult++sellers)                

generateUtilVals :: [(t, t1, b, t3)] -> [b]
generateUtilVals = map tTup 

generateSellerVals :: [MarketParticipant] -> [[(String, (String, Float, Float))]]
generateSellerVals = map (\(Seller sellerId goodInfo moneyHeld) -> map (\goodInfo -> (sellerId,goodInfo)) goodInfo)

generateOfferInput :: [a] -> [[b]] -> [(a, b)]
generateOfferInput utils sellers = concat (map (\t -> zip utils t) sellers)

generateOffers :: [(Float, (String, (String, Float, Float)))] -> [Offer]
generateOffers input = map (\(utility, (sellerId, (goodId, unitsGoodHeld,priceCharged))) -> 
        OfferInfo sellerId (goodId, unitsGoodHeld, priceCharged,utility/priceCharged)) input

generateValidOffers :: [Offer] -> [Offer]
generateValidOffers offers = [ offerInfo | offerInfo@(OfferInfo sellerId (_,unitsGoodHeld,_,_)) <- offers, unitsGoodHeld>0]

generateInvalidOffers :: [Offer] -> [Offer]
generateInvalidOffers offers = [ offerInfo | offerInfo@(OfferInfo sellerId (_,unitsGoodHeld,_,_)) <- offers, unitsGoodHeld==0]

generateUnpackedInitialResult :: Market -> [MarketParticipant]
generateUnpackedInitialResult = (\(Participants participant) -> participant)

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
generateNewSellerIdent transaction = (\(OfferInfo sellerId (_,_,_,_)) -> sellerId) (tTup(transaction))

generateRelevantSeller :: [MarketParticipant] -> String -> MarketParticipant
generateRelevantSeller sellers newseller = head [ seller | seller@(Seller sellerId _ _) <- sellers, sellerId == newseller] 

generateIrrelevantSellers :: [MarketParticipant] -> String -> [MarketParticipant]
generateIrrelevantSellers sellers newseller = [ seller | seller@(Seller sellerId _ _) <- sellers, sellerId /= newseller] 

generateRelevantSellerGoods :: MarketParticipant -> [(String, Float, Float)]
generateRelevantSellerGoods = (\(Seller sellerId goodInfo moneyHeld) -> goodInfo)

generategoodIdent :: Offer -> (String, Float, Float)
generategoodIdent = (\(OfferInfo sellerId (goodId,unitsGoodHeld,priceCharged,_)) -> (goodId,unitsGoodHeld,priceCharged))

generateIrrelevantRelevantSellerGoods :: Eq t => [t] -> t -> [t]
generateIrrelevantRelevantSellerGoods relevantSellerGoods goodIdent = [good|good<-relevantSellerGoods,good /= goodIdent]

generateNewGoods :: (t, Offer, t2, t3) -> [(String, Float, Float)] -> [(String, Float, Float)]
generateNewGoods transaction irrelevantRelevantSellerGoods =
  let newGood = (\(OfferInfo sellerId (goodId,unitsGood,priceCharged,_)) -> (goodId,unitsGood,priceCharged)) (sTup(transaction))
  in [newGood] ++ irrelevantRelevantSellerGoods 

generateNewSellers :: MarketParticipant -> [Char] -> (t1, Offer, t2, Float) -> 
        [(String, Float, Float)] -> t -> [MarketParticipant] -> [MarketParticipant] 
generateNewSellers relevantSeller newSellerIdent transaction irrelevantRelevantSellerGoods oldSellerMoney irrelevantSellers = 
 let oldSellerMoney = (\(Seller _ _ moneyHeld) -> moneyHeld) relevantSeller 
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
     oldSellerMoney = (\(Seller sellerId goodInfo moneyHeld) -> moneyHeld) relevantSeller 
 in priceFix ((fTup(transaction)):(tail(buyers))) 
        (generateNewSellers relevantSeller newSellerIdent transaction 
        irrelevantRelevantSellerGoods oldSellerMoney irrelevantSellers) result
 
priceFix:: [MarketParticipant] -> [MarketParticipant] -> Market -> EndMarket
priceFix buyers sellers result = 
 if buyers == [] 
 then generateEndMarket sellers result
 else let transaction = makeTransaction (buyerGoodInfo (head(buyers))) sellers buyers
      in if (((fTup(transaction)) == (head(buyers))) && (result == Empty)) 
         then priceFix (tail(buyers)) sellers (Participants ([(head(buyers))]))
         else if (fTup(transaction)) == (head(buyers))
              then priceFix (tail(buyers)) sellers  (Participants ([(head(buyers))]++
                (generateUnpackedInitialResult result)))
              else recursivePriceFix transaction sellers buyers result


generateSellerInput :: MarketParticipant -> [[Float]]
generateSellerInput input = 
 let goods = (\(Seller sellerId goodInfo moneyHeld) -> goodInfo) input
     purchasePrices = map (\(goodId,unitsGoodHeld,pricePaidForGood) -> pricePaidForGood) goods
     range = (map (\price -> [price..(price*pricemultiplier)]) purchasePrices)
     result = sequence range
 in result        

generateSellerInputs :: [MarketParticipant] -> [[[Float]]]
generateSellerInputs input =  map generateSellerInput input 

generateFinalInputs :: [MarketParticipant] -> [[[Float]]]
generateFinalInputs sellerinput = 
 let input = generateSellerInputs sellerinput
     result = sequence input 
 in result

getIds :: MarketParticipant -> String                                              
getIds sellers = sellerId sellers

sellerIds :: [String]
sellerIds = map getIds currentsellerinput

getGoods :: Num t => MarketParticipant -> [(String, Float, t)]
getGoods seller = map (\(goodId,unitsGoodHeld,pricePaidForGood) -> (goodId,unitsGoodHeld,-1)) (sellerGoodInfo seller)  

sellervals :: Num t => [MarketParticipant] -> [[(String, Float, t)]]
sellervals sellers= map getGoods sellers

input :: [[([Float], [(String, Float, Integer)])]]
input = map (\finalinputs -> zip finalinputs (sellervals currentsellerinput)) (generateFinalInputs currentsellerinput)

createSellerGoods :: ([Float], [(String, Float, Integer)]) -> [(String,Float,Float)]
createSellerGoods ([], _) = []
createSellerGoods (_,[]) = []
createSellerGoods (newprice:newprices, oldgood:oldgoods) = 
        [(fTup' oldgood,sTup' oldgood,newprice)] ++ createSellerGoods (newprices, oldgoods)

createSellersGoods :: [[([Float], [(String, Float, Integer)])]] -> [[[(String, Float, Float)]]]
createSellersGoods input = map (map createSellerGoods) input 

almostSellers :: [[([Float], [(String, Float, Integer)])]] -> [[(String, [(String, Float, Float)])]]
almostSellers sellersgoodsinput = map (zip sellerIds) (createSellersGoods sellersgoodsinput) 

createSeller :: (String, [(String, Float, Float)]) -> MarketParticipant
createSeller (sellerId,goodInfo) = Seller sellerId goodInfo 0

createSellers :: [[([Float], [(String, Float, Integer)])]] -> [[MarketParticipant]]
createSellers seller = map (map createSeller) (almostSellers seller) 

marketOutcomes :: [EndMarket]
marketOutcomes = map (\seller -> priceFix currentbuyerinput seller Empty) (createSellers input)

mostProfit :: Float
mostProfit = maximum (map (\(EndParticipants totalSellerProfit _ _) -> totalSellerProfit) marketOutcomes)

mostUtility :: Float
mostUtility = maximum (map (\(EndParticipants _ totalBuyerUtility _) -> totalBuyerUtility) marketOutcomes)

marketProfits :: [EndMarket]
marketProfits = [ endP | endP@(EndParticipants totalSellerProfit _ _) <- marketOutcomes, totalSellerProfit==mostProfit]

marketUtility :: [EndMarket]
marketUtility = [ endP | endP@(EndParticipants _ totalBuyerUtility _)<-marketOutcomes, totalBuyerUtility==mostUtility]                              

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




