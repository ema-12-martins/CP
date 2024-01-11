module Q1 where

import Data.List
import Cp
import List

{- Versao PointWise
matrot :: Eq a => [[a]] -> [a]
matrot [] = []
matrot (h:t) = conc(h, matrot (rotl t)) -}

rotl :: [[a]]->[[a]]
rotl = transpose . map reverse

matrot :: Eq a => [[a]] -> [a]
matrot = hyloList (either nil conc) (recList rotl.outList)