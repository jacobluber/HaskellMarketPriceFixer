module PriceMonads where 

{-
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
