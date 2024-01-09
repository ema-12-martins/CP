import Data.List
import List
import Cp

rotl :: [[a]]->[[a]]
rotl = transpose . map reverse

recList  f   = id -|- id >< rotl

matrot :: Eq a => [[a]] -> [a]
matrot = cataList  (either nil conc)
