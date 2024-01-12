module Q1 where
import Data.List
import Cp
import List

rotl :: [[a]]->[[a]]
rotl = transpose . map reverse

matrot :: Eq a => [[a]] -> [a]
matrot = hyloList (either nil conc) (recList rotl.outList)