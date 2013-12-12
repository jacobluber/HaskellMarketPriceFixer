module TestCode where

import DataTypes

currentbuyerinput = testbuyers
currentsellerinput = testsellers

testsellers = [Seller "A" [("a",4,1),("b",7,2),("c",5,3)] 0,
        Seller "B" [("a",1,3),("b",8,2),("c",2,1)] 0,
        Seller "C" [("a",0,2),("b",4,2),("c",1,2)] 0] 
testbuyers = [Buyer "Z" [("a",0,5,2),("b",0,7,1),("c",0,6,1)] 100 0, 
        Buyer "Y" [("a",0,5,4),("b",0,4,2),("c",0,2,1)] 50 0,
        Buyer "X" [("a",0,8,3),("b",0,10,3),("c",0,8,7)] 250 0] 