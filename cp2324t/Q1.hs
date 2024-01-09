import Data.List
import List
import Cp

rotl :: [[a]]->[[a]]
rotl = transpose . map reverse

matrot :: Eq a => [[a]] -> [a]
matrot = cataList (either nil conc)

matrot2 :: Eq a => [[a]] -> [a]
matrot2 [] = []
matrot2 (h:t) = h ++ matrot2 (rotl t) 
