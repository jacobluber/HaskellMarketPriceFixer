module Main where
--Haskell Market Price Fixer for Functional 2322 
--New comment!

data Util = Integer

data Money = Double

data Goods = (Price::Money, Quantity::Integer)

data Buyer = (Util, CashOnHand::Money, [Goods])

data Seller = (MinPrice::Money, [Goods])

data Market = ([Buyer], [Seller])

priceFix :: Market a -> Market a

