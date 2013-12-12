module Helper where

import DataTypes
import TestCode

fTup (x,_,_,_) = x
sTup (_,x,_,_) = x
tTup (_,_,x,_) = x
frTup (_,_,_,x) = x
fTup' (x,_,_) = x
sTup' (_,x,_) = x
tTup' (_,_,x) = x
