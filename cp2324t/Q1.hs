import Data.List
import List
import Cp

rotl :: [[a]]->[[a]]
rotl = transpose . map reverse

matrot :: Eq a => [[a]] -> [a]
matrot = cataList (either nil  (conc . (id >< (matrot . rotl))))
