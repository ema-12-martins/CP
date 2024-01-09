import Data.List
import Cp
import List (outList)


rotl :: [[a]]->[[a]]
rotl = transpose . map reverse


{- matrot2 :: Eq a => [[a]] -> [a]
matrot2 [] = []
matrot2 (h:t) = conc(h, matrot2 (rotl t)) -}

matrot2 :: Eq a => [[a]] -> [a]
matrot2 = either nil (conc . (id >< matrot2).(id><rotl)) . outList