data Product = Product {name :: String, price :: Float, quantity :: Integer} der
iving (Show)
data Buyer = Buyer {cash :: Float, info :: [([Char], Float)], totalUtil :: Float, purchased :: [Product]} deriving (Show)
data Seller = Seller {products :: [Product]} deriving (Show)
data Market = Market {buyers :: [Buyer], sellers :: [Seller]} | Empty deriving (Show)
--test