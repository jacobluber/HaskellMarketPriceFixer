module Main where
import Markets
import Helper
import TestCode 
import RandomM
import System.IO
import System.Environment
import Control.Monad
import System.Console.GetOpt

data Flag = Number Int | UtilVendor | Total | Help | Profit | Utility deriving Eq

stringToMultiple Nothing = Number 1000
stringToMultiple (Just s) = Number $ read s

options = [Option ['t'] ["total"] (NoArg Total) "Output total profit and utility from best pricing scheme",
           Option ["uv"] ["utils-vendor"] (NoArg UtilVendor) "Insert util vendor into calculation of best pricing scheme",
           Option ['n'] ["num"] (OptArg stringToMultiple "num") "Search up to <num> possible pricing schemes", 
           Option ['h'] ["help"] (NoArg Help) "Show flag descriptions",
           Option ["profit"] (NoArg Profit) "Maximize profit and ignore utility",
           Option ["utility"] (NoArg Utility) "Maximize utility and ignore profit"]

noRepeat :: Flag -> Bool
noRepeat (Number _) = True
noRepeat UtilVendor = True
noRepeat Total = True
noRepeat Profit = True
noRepeat Utility = True
noRepeat _ = False

main = do
       args <- getArgs
       let (flags, fileargs, errors) = getOpt Permute options args :: ([Flag], [String], [String])
       if length fileargs == 0 || Help `elem` flags
       then putStrLn (usageInfo "Usage: Market Project [OPTION]...FILE\nGenerates market." options)
       else do
           let repeat = not (any noRepeat flags)
           if Profit `elem` flags
           then show marketProfits
           if Utility `elem` flags
           then show marketUtility
           when repeat (do
                putStrLn "Generate another market?"
                response <- getLine
                if response `elem` ["Yes", "yes", "y", "Y", "Okay", "okay", "ok", "OK", "yep", "Yep"]
                then main
                else return())

{--
main :: IO() 
main = do
putStrLn ("For the market of buyers and sellers represented by: ")
putStrLn ("Buyers: "++show testbuyers)
putStrLn ("Sellers: "++show testsellers)
putStrLn (" ")
putStrLn ("The algorithm has tested "++show (length marketOutcomes)++" different market outcomes.")
putStrLn ("Of these outcomes, the one(s) that get(s) the most seller profit("++(show mostProfit)++") is/are: ")
putStrLn (show marketProfits)
putStrLn (" ")
putStrLn ("Of these outcomes, the one(s) that get(s) the most buyer utility("++(show mostUtility)++") is/are: ")
putStrLn (show marketUtility)-}

