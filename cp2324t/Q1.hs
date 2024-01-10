module Q1 where
    
import Data.List
import Cp
import List (outList)


rotl :: [[a]]->[[a]]
rotl = transpose . map reverse


{- matrot :: Eq a => [[a]] -> [a]
matrot [] = []
matrot (h:t) = conc(h, matrot (rotl t)) -}

matrot :: Eq a => [[a]] -> [a]
matrot = either nil (conc.(id >< matrot).(id><rotl)).outList